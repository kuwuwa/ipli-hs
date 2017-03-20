module Prolog.Parser (
    TokenStream(..)
  , SParser
  , topLevel
  , expr
  , func
  , prim
  , expr0
  , upperPrecLimit
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Lib.Parser (ParserT(..), parserT, runParserT, Result(..), failParse)

import Prolog.Token (Token)
import qualified Prolog.Token as Tk
import Prolog.AstNode (AstNode(..))
import Prolog.Operator (Operator(..), OpState, OpType(..), OpData(..))

import Debug.Trace

------------------------------------------------------------
-- token stream
------------------------------------------------------------

type Index = Int
data TokenStream = TokenStream Index [Token]

instance Show TokenStream where
  show (TokenStream ind xs) = "TokenStream[" ++ show ind ++ "]" ++ show xs

instance Eq TokenStream where
  TokenStream ind0 xs0 == TokenStream ind1 xs1 = (ind0, xs0) == (ind1, xs1)

------------------------------------------------------------
-- type definition of syntactic parser
------------------------------------------------------------

type SParser = ParserT TokenStream (StateT OpData ParseState)

------------------------------------------------------------
-- memoization
------------------------------------------------------------

type Prec = Int

type ParseMemo = Map (Index, Prec) (Result AstNode, TokenStream)

type ParseState = State ParseMemo

setMemo :: (Index, Prec) -> SParser AstNode -> SParser AstNode
setMemo key parser = parserT $ \st -> do
  result <- runParserT parser st
  lift . modify $ Map.insert key result
  return result

withMemo :: (Index, Prec) -> SParser AstNode -> SParser AstNode
withMemo key parser = do
  memo <- lift . lift . gets $ Map.lookup key
  case memo of
    Nothing -> setMemo key parser
    Just res -> setResult res
  where setResult res = parserT $ \_ -> return res
  
----------------------------------------------------------
-- syntactic parsers for Prolog
----------------------------------------------------------

upperPrecLimit = 1200
lowerPrecLimit = 0

topLevel :: SParser AstNode
topLevel = do
  e <- expr upperPrecLimit
  period_
  return e

expr :: Prec -> SParser AstNode
expr prec = do
  ind <- index
  result <- withMemo (ind, prec) $ do
    lookahead
    l0 <- lassoc
    yf l0 <|> return l0
  return result
  where lookahead = parserT $ \st -> do
          (result, _) <- runParserT anything st
          return $ case result of
                     Fail msg           -> (Fail $ "no more token", st)
                     OK Tk.RParen       -> (Fail $ "unexpected `)'", st)
                     OK Tk.RBracket     -> (Fail $ "unexpected `]'", st)
                     OK Tk.Bar          -> (Fail $ "unexpected bar", st)
                     v                  -> (v, st)

        term = fx <|> fy <|> xfx <|> suffix (lowerExpr prec)
          where fx = do
                  (Operator name _ _) <- oper Fx prec
                  term <- suffix $ lowerExpr prec
                  return $ Func name [term]
                fy = do
                  (Operator name _ _) <- oper Fy prec
                  term <- term
                  return $ Func name [term]

        xfx = do
          lhs <- lowerExpr prec
          (Operator name _ _) <- oper Xfx prec
          rhs <- lowerExpr prec
          return $ Func name [lhs, rhs]

        suffix parser = parser >>= loop
          where loop term = xf term <|> yf term <|> return term

        xf term = do
          (Operator name _ _) <- oper Xf prec
          return $ Func name [term]

        yf term = do
          (Operator name _ _) <- oper Yf prec
          let term' = Func name [term]
          yf term' <|> return term'

        rassoc = loop <|> term
          where loop = do
                  lhs <- lowerExpr prec
                  (Operator name _ _) <- oper Xfy prec
                  rhs <- rassoc
                  return $ Func name [lhs, rhs]

        lassoc = do
          lhs <- rassoc
          loop lhs
          where loop ter = (do
                  (Operator name _ _) <- oper Yfx prec
                  rhs <- lowerExpr prec
                  loop $ Func name [ter, rhs]) <|> return ter

expr0 = do
  ind <- index
  withMemo (ind, 0) $ withParen (expr upperPrecLimit) <|>
                      func <|>
                      prim <|>
                      list <|>
                      failParse "not an expression"

func :: SParser AstNode
func = do
  pred <- prim
  case pred of
    Atom a -> do
      xs <- do
        lparen
        args <- (do
          arg0 <- lowerExpr 1000
          rest <- many $ do
            commaSep
            lowerExpr 1000
          return $ arg0:rest) <|> return []
        rparen <|> failParse "expected a close parenthesis"
        return $ args
      return $ Func a xs
    _ -> failParse "not a func"

list :: SParser AstNode
list = do
  lbracket
  (<|>) (rbracket >> (return Nil)) $ do
    v <- lowerExpr 1000
    vs <- many $ do
      commaSep
      lowerExpr 1000
    w <- (exactToken Tk.Bar >> lowerExpr 1000) <|> return Nil
    rbracket
    return $ foldr Pair w (v:vs)

prim :: SParser AstNode
prim = do
  token <- anything
  case token of
    Tk.Atom a b -> return $ Atom a
    Tk.Var    v -> return $ Var v
    Tk.PInt   i -> return $ PInt i
    Tk.PFloat f -> return $ PFloat f
    Tk.Str    s -> return $ Str s
    _           -> failParse "not a primitive expression"

------------------------------------------------------------
-- helpful parsers
------------------------------------------------------------

lowerExpr :: Int -> SParser AstNode
lowerExpr prec = do
  prec' <- lift . gets $ Set.lookupLT prec . precs
  case prec' of
    Nothing -> expr0
    Just prec' -> expr prec'

index :: SParser Index
index = parserT $ \st@(TokenStream ind xs) -> return (OK ind, st)

commaSep = do
  a <- anything
  case a of
    Tk.Atom "," False -> return ()
    _ -> failParse "not a comma separator"

oper :: OpType -> Int -> SParser Operator
oper opType prec = do 
  token <- anything
  case token of
    Tk.Atom name False -> do
      op <- getOp name
      case op of
        Nothing -> failParse "not an operator"
        Just (Operator name prec' opType')
          | opType == opType' && prec == prec' -> return $ Operator name prec opType
          | otherwise         -> failParse $ "not " ++ show opType ++ " operator"
    _ -> failParse "not an operator"
  where getOp
          | opType `elem` [Fx, Fy] = getFz
          | opType `elem` [Xf, Yf] = getZf
          | otherwise = getZfz

        getZfz :: String -> SParser (Maybe Operator)
        getZfz key = lift . gets $ Map.lookup key . zfzMap

        getFz :: String -> SParser (Maybe Operator)
        getFz key = lift . gets $ Map.lookup key . fzMap

        getZf :: String -> SParser (Maybe Operator)
        getZf key = lift . gets $ Map.lookup key . zfMap

withParen :: SParser AstNode -> SParser AstNode
withParen p = do
  lparen
  val <- p
  rparen
  return val

exactToken :: Token -> SParser Token
exactToken target = do
  tk <- anything
  if tk == target
    then return tk
    else failParse $ "expected " ++ show target ++ ", but actually " ++ show tk

------------------------------------------------------------
-- some atomic parsers
------------------------------------------------------------

anything :: SParser Token
anything = parserT $ return . p
  where p st@(TokenStream _ [])       = (Fail "no more token", st)
        p (TokenStream ind (x:xs)) = (OK x, TokenStream (ind+1) xs)

lparen :: SParser Token
lparen = exactToken Tk.LParen

rparen :: SParser Token
rparen = exactToken Tk.RParen

lbracket :: SParser Token
lbracket = exactToken Tk.LBracket

rbracket :: SParser Token
rbracket = exactToken Tk.RBracket

period :: SParser Token
period = exactToken Tk.Period

period_ :: SParser ()
period_ = period >> return ()
