module Prolog.Parser (
    TokenStream(..)
  , PLParserT
  , PLParser
  , runPLParserT
  , runPLParser
  , liftPLParserT
  , topLevel
  , expr
  , anything
  , lowerPrecLimit
  , upperPrecLimit
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State

import           Data.Functor.Identity

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set

import           Lib.Parser (ParserT(..), runParserT, Result(..), failParse)

import           Prolog.Token    (Token)
import qualified Prolog.Token    as Tk
import           Prolog.Node  (Node(..))
import           Prolog.Operator (Operator(..), OpState, OpType(..), OpData(..))

import           Debug.Trace

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

type PLParserT m = ParserT TokenStream (StateT OpData (StateT ParseMemo m))

type PLParser = PLParserT Identity

runPLParserT :: Monad m =>
  PLParserT m a -> TokenStream -> OpData -> m (Result a, TokenStream, OpData)
runPLParserT p tokens opD = do
  (((result, tokens'), opD'), _) <- runStateT (runStateT (runParserT p tokens) opD) Map.empty
  return (result, tokens', opD')

runPLParser :: PLParserT Identity a -> TokenStream -> OpData -> (Result a, TokenStream, OpData)
runPLParser p tokens opD = runIdentity (runPLParserT p tokens opD)

liftPLParserT :: Monad m => m a -> PLParserT m a
liftPLParserT = lift . lift . lift

------------------------------------------------------------
-- memoization
------------------------------------------------------------

type Prec = Int

type ParseMemo = Map (Index, Prec) (Result Node, TokenStream)

type ParseState = State ParseMemo

setMemo :: Monad m => (Index, Prec) -> PLParserT m Node -> PLParserT m Node
setMemo key parser = ParserT $ \st -> do
  result <- runParserT parser st
  lift . modify $ Map.insert key result
  return result

withMemo :: Monad m => (Index, Prec) -> PLParserT m Node -> PLParserT m Node
withMemo key parser = do
  memo <- lift . lift . gets $ Map.lookup key
  case memo of
    Nothing -> setMemo key parser
    Just res -> setResult res
  where setResult res = ParserT $ \_ -> return res
  
----------------------------------------------------------
-- syntactic parsers for Prolog
----------------------------------------------------------

upperPrecLimit = 1200
lowerPrecLimit = 0

topLevel :: Monad m => PLParserT m Node
topLevel = do
  e <- expr upperPrecLimit
  period_
  return e

expr :: Monad m => Prec -> PLParserT m Node
expr prec = do
  ind <- index
  result <- withMemo (ind, prec) $ do
    lookahead
    l0 <- lassoc
    yf l0 <|> return l0
  return result
  where lookahead = ParserT $ \st -> do
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

expr0 :: Monad m => PLParserT m Node
expr0 = do
  ind <- index
  withMemo (ind, 0) $ withParen (expr upperPrecLimit) <|>
                      func <|>
                      prim <|>
                      list <|>
                      failParse "not an expression"

func :: Monad m => PLParserT m Node
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

list :: Monad m => PLParserT m Node
list = do
  lbracket
  (<|>) (rbracket >> (return Nil)) $ do
    v <- lowerExpr 1000
    vs <- many $ do
      commaSep
      lowerExpr 1000
    w <- (exactToken Tk.Bar >> lowerExpr 1000) <|> return Nil
    rbracket
    return $ foldr (\a b -> Func "[|]" [a,b]) w (v:vs)

prim :: Monad m => PLParserT m Node
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

lowerExpr :: Monad m => Int -> PLParserT m Node
lowerExpr prec = do
  prec' <- lift . gets $ Set.lookupLT prec . precs
  case prec' of
    Nothing -> expr0
    Just prec' -> expr prec'

index :: Monad m => PLParserT m Index
index = ParserT $ \st@(TokenStream ind xs) -> return (OK ind, st)
 
commaSep :: Monad m => PLParserT m ()
commaSep = do
  a <- anything
  case a of
    Tk.Atom "," False -> return ()
    _ -> failParse "not a comma separator"

oper :: Monad m => OpType -> Int -> PLParserT m Operator
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

        getZfz :: Monad m => String -> PLParserT m (Maybe Operator)
        getZfz key = lift . gets $ Map.lookup key . zfzMap

        getFz :: Monad m => String -> PLParserT m (Maybe Operator)
        getFz key = lift . gets $ Map.lookup key . fzMap

        getZf :: Monad m => String -> PLParserT m (Maybe Operator)
        getZf key = lift . gets $ Map.lookup key . zfMap

withParen :: Monad m => PLParserT m Node -> PLParserT m Node
withParen p = do
  lparen
  val <- p
  rparen
  return val

exactToken :: Monad m => Token -> PLParserT m Token
exactToken target = do
  tk <- anything
  if tk == target
    then return tk
    else failParse $ "expected " ++ show target ++ ", but actually " ++ show tk

------------------------------------------------------------
-- some atomic parsers
------------------------------------------------------------

anything :: Monad m => PLParserT m Token
anything = ParserT $ return . p
  where p st@(TokenStream _ [])       = (Fail "no more token", st)
        p (TokenStream ind (x:xs)) = (OK x, TokenStream (ind+1) xs)

lparen :: Monad m => PLParserT m Token
lparen = exactToken Tk.LParen

rparen :: Monad m => PLParserT m Token
rparen = exactToken Tk.RParen

lbracket :: Monad m => PLParserT m Token
lbracket = exactToken Tk.LBracket

rbracket :: Monad m => PLParserT m Token
rbracket = exactToken Tk.RBracket

period :: Monad m => PLParserT m Token
period = exactToken Tk.Period

period_ :: Monad m => PLParserT m ()
period_ = period >> return ()
