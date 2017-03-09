module Lib.StringParser (
    StrStream(..)
  , Pos(..)
  , beginNum
  , char
  , line
  , exact
  , oneOfChars
  , upper
  , lower
  , digit
  , space
  , spaces
  , consume
  ) where

import Control.Monad
import Control.Applicative
import Data.Char

import Lib.Parser

type StrParser o = Parser StrStream o

data StrStream = StrStream String Pos

instance Eq StrStream where
  StrStream s0 p0 == StrStream s1 p1 = s0 == s1 && p0 == p1

instance Show StrStream where
  show (StrStream str (Pos line col)) =
    "StrStream{line: "  ++ show line ++ ", col: " ++ show col ++
    ", current: \"" ++ str ++ "\"}"

--------------------

type Line = Int
type Column = Int
data Pos = Pos Line Column

instance Show Pos where
  show (Pos line col) = "Pos " ++ show line ++ " " ++ show col

instance Eq Pos where
  Pos lx cx == Pos ly cy = lx == ly && cx == cy

beginNum :: Int
beginNum = 0

proceed :: Int -> Pos -> Pos
proceed n (Pos line col) = Pos line (col+n)

proceed1 :: Pos -> Pos
proceed1 = proceed 1

newLine :: Pos -> Pos
newLine (Pos line col) = Pos (line+1) beginNum

--------------------

isNewLine :: Char -> Bool
isNewLine = flip elem "\n"

char :: StrParser Char
char = parser $ p
  where p st@(StrStream [] pos)  = (Fail "no more character", st)
        p (StrStream (x:xs) pos) =
          let nextPos = (if isNewLine x then newLine else proceed1) pos
          in (OK x, StrStream xs nextPos)

line :: StrParser String
line = do
  x <- char
  if isNewLine x
    then return []
    else fmap (x:) line


exact :: Char -> StrParser Char
exact c = filterP (== c) ("not " ++ [c]) char

oneOfChars :: [Char] -> StrParser Char
oneOfChars = foldl (<|>) failParser . map exact

upper :: StrParser Char
upper = filterP isAsciiUpper "not one of A-Z" char

lower :: StrParser Char
lower = filterP isAsciiLower "not one of a-z" char

digit :: StrParser Char
digit = filterP isDigit "not a digit" char

space :: StrParser ()
space = filterP (== ' ') "not a space" char >> return ()

spaces :: StrParser ()
spaces = many space >> return ()

filterP :: (a -> Bool) -> String -> StrParser a -> StrParser a
filterP pred msg p = parser $ \st ->
  let (res, st') = runParser p st in
    case (res, fmap pred res) of
      (Fail _, _)   -> (res, st)
      (_, OK False) -> (Fail msg, st)
      _             -> (res, st')

consume :: StrParser a -> StrParser String
consume p = parser $ \st ->
  let (res, st') = runParser p st in
    case res of
      Fail msg -> (Fail msg, st')
      OK v     -> (OK $ collect st st', st')
  where collect st@(StrStream _ posX) dest@(StrStream _ posY)
          | posX == posY = []
          | otherwise = let (OK c, st') = runParser char st
                        in c : collect st' dest
