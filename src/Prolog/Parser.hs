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

import Lib.Parser (ParserT(..), runParserT, Result(..), failParse)

import Prolog.Token (Token)
import qualified Prolog.Token as Tk
import Prolog.AstNode (AstNode(..))
import Prolog.Operator (Operator(..), OpState, OpType(..), OpMap(..), OpData(..))

import Debug.Trace

------------------------------
-- type definition of syntactic parser
------------------------------

type SParser = ParserT [Token] OpState

getZfz :: String -> SParser (Maybe Operator)
getZfz key = lift . gets $ Map.lookup key . zfzMap

getFz :: String -> SParser (Maybe Operator)
getFz key = lift . gets $ Map.lookup key . fzMap

getZf :: String -> SParser (Maybe Operator)
getZf key = lift . gets $ Map.lookup key . zfMap

nextPrec :: Int -> SParser (Maybe Int)
nextPrec prec = lift . gets $ Set.lookupLT prec . precs

------------------------------

upperPrecLimit = 1200
lowerPrecLimit = 0

-- topLevel :: SParser AstNode
-- topLevel = do
--
topLevel :: SParser AstNode
topLevel = do
  e <- expr upperPrecLimit
  period_
  return e

expr :: Int -> SParser AstNode
expr prec = do
  -- seq (trace ("prec: " ++ show prec) prec) (return ())
  xfx <|> suffix lassoc <|> lowerExpr
  where xfx = do
          lhs <- lowerExpr
          (Operator name _ _) <- oper Xfx prec
          rhs <- lowerExpr
          return $ Comp name [lhs, rhs]

        lowerExpr = do
          val <- nextPrec prec
          case val of
            Nothing -> withParen (expr upperPrecLimit) <|> compound <|> prim <|> failParse "not a expression"
            Just prec' -> expr prec'

        prefix = fx <|> fy <|> suffix lowerExpr
          where fx = do
                  (Operator name _ _) <- oper Fx prec
                  term <- suffix lowerExpr
                  return $ Comp name [term]
                fy = do
                  (Operator name _ _) <- oper Fy prec
                  term <- prefix
                  return $ Comp name [term]

        suffix parser = parser >>= loop
          where loop term = xf term <|> yf term <|> return term
                xf term = do
                  (Operator name _ _) <- oper Xf prec
                  return $ Comp name [term]
                yf term = do
                  (Operator name _ _) <- oper Yf prec
                  loop $ Comp name [term]

        rassoc = loop <|> prefix
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

------------------------------
-- parser combinators for syntactic parsing
------------------------------

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

------------------------------
-- some atomic parsers
------------------------------

anything :: SParser Token
anything = ParserT $ return . p
  where p [] =     (Fail "no more token", [])
        p (x:xs) = (OK x, xs)

lparen :: SParser Token
lparen = exactToken Tk.LParen

rparen :: SParser Token
rparen = exactToken Tk.RParen

period :: SParser Token
period = exactToken (Tk.Atom "." True) <|> exactToken (Tk.Atom "." False)

period_ :: SParser ()
period_ = period >> return ()
