module Prolog.Parser where
-- module Prolog.Parser (
--     SParser
--   , topLevel
--   ) where

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
import Prolog.Operator (Operator(..), OpState, OpType(..), OpMap(..), OpData(..))

import Debug.Trace

------------------------------------------------------------
-- token stream
------------------------------------------------------------

type Index = Int
data TokenStream = TokenStream Index [Token]

instance Show TokenStream where
  show (TokenStream ind xs) = "TokenStream[" ++ show ind ++ "]" ++ show xs

----------------------------------------------------------
-- type definition of syntactic parser
----------------------------------------------------------

type SParser = ParserT TokenStream (StateT OpData ParseState)

getZfz :: String -> SParser (Maybe Operator)
getZfz key = lift . gets $ Map.lookup key . zfzMap

getFz :: String -> SParser (Maybe Operator)
getFz key = lift . gets $ Map.lookup key . fzMap

getZf :: String -> SParser (Maybe Operator)
getZf key = lift . gets $ Map.lookup key . zfMap

nextPrec :: Int -> SParser (Maybe Int)
nextPrec prec = lift . gets $ Set.lookupLT prec . precs

----------------------------------------------------------
-- memoization
----------------------------------------------------------

type Prec = Int

type ParseMemo = Map (Index, Prec) (Result AstNode, TokenStream)

type ParseState = State ParseMemo

getMemo :: (Index, Prec) -> SParser AstNode
getMemo key = do
  memo <- lift . lift . gets $ Map.lookup key
  case memo of
    Nothing -> failParse $ "couldn't remember"
    Just res -> setResult res
  where setResult res = parserT $ \_ -> return res

setMemo :: (Index, Prec) -> SParser AstNode -> SParser AstNode
setMemo key parser = parserT $ \st -> do
  result <- runParserT parser st
  lift . modify $ Map.insert key result
  return result

withMemo :: (Index, Prec) -> SParser AstNode -> SParser AstNode
withMemo key parser = getMemo key <|> setMemo key parser
  
----------------------------------------------------------

upperPrecLimit = 1200
lowerPrecLimit = 0

topLevel :: SParser AstNode
topLevel = do
  e <- expr upperPrecLimit
  period_
  return e

index :: SParser Index
index = parserT $ \st@(TokenStream ind xs) -> return (OK ind, st)

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
                     Fail msg -> (Fail $ "(lookahead)" ++ msg, st)
                     v        -> (v, st)

        lowerExpr = do
          ind <- index
          val <- nextPrec prec
          case val of
            Nothing -> withMemo (ind, 0) simpleExpr
            Just prec' -> expr prec'

        simpleExpr = withParen (expr upperPrecLimit) <|> compound <|> prim <|> failParse "not an expression"

        term = fx <|> fy <|> xfx <|> suffix lowerExpr
          where fx = do
                  (Operator name _ _) <- oper Fx prec
                  term <- suffix lowerExpr
                  return $ Comp name [term]
                fy = do
                  (Operator name _ _) <- oper Fy prec
                  term <- term
                  return $ Comp name [term]

        xfx = do
          lhs <- lowerExpr
          (Operator name _ _) <- oper Xfx prec
          rhs <- lowerExpr
          return $ Comp name [lhs, rhs]

        suffix parser = parser >>= loop
          where loop term = xf term <|> yf term <|> return term

        xf term = do
          (Operator name _ _) <- oper Xf prec
          return $ Comp name [term]
        yf term = do
          (Operator name _ _) <- oper Yf prec
          let term' = Comp name [term]
          yf term' <|> return term'

        rassoc = loop <|> term
          where loop = do
                  lhs <- lowerExpr
                  (Operator name _ _) <- oper Xfy prec
                  rhs <- rassoc
                  return $ Comp name [lhs, rhs]

        lassoc = do
          lhs <- rassoc
          loop lhs
          where loop ter = (do
                  (Operator name _ _) <- oper Yfx prec
                  rhs <- lowerExpr
                  loop $ Comp name [ter, rhs]) <|> return ter

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

compound :: SParser AstNode
compound = do
  pred <- prim
  case pred of
    Atom a -> do
      xs <- do
        lparen
        args <- (do
          arg0 <- expr 999
          rest <- many $ do
            commaSep
            expr 999
          return $ arg0:rest) <|> return []
        rparen <|> failParse "expected a close parenthesis"
        return $ args
      return $ Comp a xs
    _ -> failParse "not a compound"
  where commaSep = do
          a <- anything
          case a of
            Tk.Atom "," False -> return ()
            _ -> failParse "not a comma separator"
    
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

----------------------------------------------------------
-- parser combinators for syntactic parsing
----------------------------------------------------------

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

----------------------------------------------------------
-- some atomic parsers
----------------------------------------------------------

anything :: SParser Token
anything = parserT $ return . p
  where p st@(TokenStream _ [])       = (Fail "no more token", st)
        p (TokenStream ind (x:xs)) = (OK x, TokenStream (ind+1) xs)

lparen :: SParser Token
lparen = exactToken Tk.LParen

rparen :: SParser Token
rparen = exactToken Tk.RParen

period :: SParser Token
period = exactToken Tk.Period

period_ :: SParser ()
period_ = period >> return ()
