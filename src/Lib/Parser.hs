module Lib.Parser
  ( Pos(..)
  , Result(..)
  , Parser(..)
  , PState(..)
  , parser
  , newLine
  , proceed
  , proceed1
  , beginPos
  , failParser
  , parseFail
  ) where

import Control.Applicative
import Control.Monad

type Line = Int
type Column = Int

--------------------

data Pos = Pos Line Column

instance Show Pos where
  show (Pos line col) = "Pos " ++ show line ++ " " ++ show col

instance Eq Pos where
  Pos lx cx == Pos ly cy = lx == ly && cx == cy

begin :: Int
begin = 0

beginPos = Pos begin begin

proceed :: Int -> Pos -> Pos
proceed n (Pos line col) = Pos line (col+n)

proceed1 :: Pos -> Pos
proceed1 = proceed 1

newLine :: Pos -> Pos
newLine (Pos line col) = Pos (line+1) begin

--------------------

data PState = PState String Pos

instance Show PState where
  show (PState str (Pos line col)) =
    "PState[line: "  ++ show line ++ ", col: " ++ show col ++
    ", current: \"" ++ str ++ "\"]"

--------------------

type Msg = String

data Result o = Fail Msg | OK o

instance (Show o) => Show (Result o) where
  show (Fail msg) = "Fail " ++ show msg
  show (OK o) = "OK " ++ show o

instance Functor Result where
  fmap f (Fail msg) = Fail msg
  fmap f (OK v) = OK (f v)

--------------------

data Parser o = Parser { runParser :: PState -> (Result o, PState)  }

parser :: (PState -> (Result o, PState)) -> Parser o
parser = Parser

failParser :: Parser a
failParser = parser $ \st -> (Fail "failParser", st)

instance Functor Parser where
  fmap f p = parser $ \st ->
    case runParser p st of
      (OK o, st')     -> (OK (f o), st')
      (Fail msg, st') -> ((Fail msg), st')

instance Applicative Parser where
  pure x = parser $ \st -> (OK x, st)
  y <*> x = parser $ \st ->
    let (v, st') = runParser x st
        (w, st'') = runParser y st'
        result = case (v, w) of
                   (OK o, OK f)   -> OK (f o)
                   (Fail msg, _)  -> Fail msg
                   (_, Fail msg') -> Fail msg'
    in (result, st'')

instance Alternative Parser where
  empty = failParser
  (<|>) p q = parser $ \st ->
    let (res, st') = runParser p st in
      case res of
        (OK _) -> (res, st')
        _      -> runParser q st

instance Monad Parser where
  return = pure
  x >>= f = parser $ \st ->
    let (y, st') = runParser x st in
      case y of Fail msg -> (Fail msg, st')
                OK v     -> runParser (f v) st'

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

parseFail :: String -> Parser a
parseFail msg = parser $ \st -> (Fail msg, st)
