module Lib.Parser
  ( Pos
  , Result(..)
  , Parser(..)
  , PState(..)
  , beginPos
  , char
  , digit
  , parser
  , failParser
  , parseFail
  , line
  , exact
  , lower
  , upper
  , space
  , spaces
  ) where

import Control.Applicative
import Control.Monad
import Data.Char

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

proceed1 :: Pos -> Pos
proceed1 (Pos line col) = Pos line (col+1)

proceed :: Int -> Pos -> Pos
proceed n (Pos line col) = Pos line (col+n)

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

--------------------

parseFail :: String -> Parser a
parseFail msg = parser $ \st -> (Fail msg, st)

filterP :: (a -> Bool) -> String -> Parser a -> Parser a
filterP pred msg p = parser $ \st -> let (res, st') = runParser p st in
    case (res, fmap pred res) of
      (Fail _, _)   -> (res, st)
      (_, OK False) -> (Fail msg, st)
      _             -> (res, st')

--------------------

isNewLine :: Char -> Bool
isNewLine = flip elem "\n"

char :: Parser Char
char = parser $ p
  where p st@(PState [] pos)  = (Fail "no more character", st)
        p (PState (x:xs) pos) =
          let nextPos = (if isNewLine x then newLine else proceed1) pos
          in (OK x, PState xs nextPos)

line :: Parser String
line = do
  x <- char
  if isNewLine x
  then return [x]
  else do
    xs <- line
    return (x:xs)

exact :: Char -> Parser Char
exact c = filterP (== c) ("not " ++ [c]) char

upper :: Parser Char
upper = filterP isAsciiUpper "not one of A-Z" char

lower :: Parser Char
lower = filterP isAsciiLower "not one of a-z" char

digit :: Parser Char
digit = filterP isDigit "not a digit" char

space :: Parser ()
space = filterP (== ' ') "not a space" char >> return ()

spaces :: Parser ()
spaces = many space >> return ()
