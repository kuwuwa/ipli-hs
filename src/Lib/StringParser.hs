module Lib.StringParser (
    char
  , line
  , exact
  , oneOfChars
  , upper
  , lower
  , digit
  , space
  , spaces
  ) where

import Control.Monad
import Control.Applicative
import Data.Char

import Lib.Parser

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
    then return []
    else fmap (x:) line


exact :: Char -> Parser Char
exact c = filterP (== c) ("not " ++ [c]) char

oneOfChars :: [Char] -> Parser Char
oneOfChars = foldl (<|>) failParser . map exact

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

filterP :: (a -> Bool) -> String -> Parser a -> Parser a
filterP pred msg p = parser $ \st ->
  let (res, st') = runParser p st in
    case (res, fmap pred res) of
      (Fail _, _)   -> (res, st)
      (_, OK False) -> (Fail msg, st)
      _             -> (res, st')
