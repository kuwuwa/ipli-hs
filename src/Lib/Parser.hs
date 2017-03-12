module Lib.Parser
  ( Result(..)
  , Parser(..)
  , parser
  , failParser
  , failParse
  ) where

import Control.Applicative
import Control.Monad

type Msg = String

data Result o = Fail Msg | OK o
instance (Eq o) => Eq (Result o) where
  Fail a == Fail b = a == b
  OK a   == OK b   = a == b
  _      == _      = False

instance (Show o) => Show (Result o) where
  show (Fail msg) = "Fail " ++ show msg
  show (OK o) = "OK " ++ show o

instance Functor Result where
  fmap f (Fail msg) = Fail msg
  fmap f (OK v) = OK (f v)

--------------------

data Parser s o = Parser { runParser :: s -> (Result o, s)  }

parser :: (s -> (Result o, s)) -> Parser s o
parser = Parser

failParser :: Parser s a
failParser = parser $ \st -> (Fail "failParser", st)

instance Functor (Parser s) where
  fmap f p = parser $ \st ->
    case runParser p st of
      (OK o,     st') -> (OK (f o), st')
      (Fail msg, _) -> (Fail msg, st)

instance Applicative (Parser s) where
  pure x = parser $ \st -> (OK x, st)
  x <*> y = parser $ \st ->
    case runParser x st of
      (Fail msg, _) -> (Fail msg, st)
      (OK f, st') -> case runParser y st' of
                       (Fail msg, _) -> (Fail msg, st')
                       (OK x,  st'') -> (OK $ f x, st'')

instance Alternative (Parser s) where
  empty = failParser
  p <|> q = parser $ \st ->
    case runParser p st of
      (Fail _, _) -> runParser q st
      (res,  st') -> (res, st')
  many p = parser $ \st ->
    case runParser p st of
      (Fail msg, _) -> (OK [], st)
      (OK v, st') -> let (OK vs, st'') = runParser (many p) st' in (OK (v:vs), st'')
  some p = fmap (:) p <*> many p

instance Monad (Parser s) where
  return = pure
  x >>= f = parser $ \st ->
    let (y, st') = runParser x st in
      case y of Fail msg -> (Fail msg, st')
                OK v     -> runParser (f v) st'

instance MonadPlus (Parser s) where
  mzero = empty
  mplus = (<|>)

failParse :: String -> Parser s a
failParse msg = parser $ \st -> (Fail msg, st)
