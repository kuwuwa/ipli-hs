module Lib.Parser.String (
  StrParser,
  StrState(..),
  Pos(..),
  beginPos,
  beginNum,
  char,
  line,
  exact,
  oneOfChars,
  upper,
  lower,
  digit,
  space,
  spaces,
  consume,
  ) where

import           Lib.Parser
import           Lib.Parser.Combinator

import           Control.Monad
import           Control.Applicative

import           Data.Char

type StrParser o = Parser StrState o

data StrState = StrState String Pos

instance Eq StrState where
  StrState s0 p0 == StrState s1 p1 = s0 == s1 && p0 == p1

instance Show StrState where
  show (StrState str (Pos ln col)) =
    "StrState{line: "  ++ show ln ++ ", col: " ++ show col ++
    ", current: \"" ++ str ++ "\"}"

--------------------

type Line = Int
type Column = Int
data Pos = Pos Line Column

instance Show Pos where
  show (Pos ln col) = "Pos " ++ show ln ++ " " ++ show col

instance Eq Pos where
  Pos lx cx == Pos ly cy = lx == ly && cx == cy

beginNum :: Int
beginNum = 0

beginPos :: Pos
beginPos = Pos beginNum beginNum

proceed :: Int -> Pos -> Pos
proceed n (Pos ln col) = Pos ln (col+n)

proceed1 :: Pos -> Pos
proceed1 = proceed 1

newLine :: Pos -> Pos
newLine (Pos ln _) = Pos (ln+1) beginNum

--------------------

isNewLine :: Char -> Bool
isNewLine = flip elem "\n"

char :: StrParser Char
char = parser $ p
  where p st@(StrState [] _)  = (Fail "no more character", st)
        p (StrState (x:xs) pos) =
          let nextPos = (if isNewLine x then newLine else proceed1) pos
          in (OK x, StrState xs nextPos)

line :: StrParser String
line = do
  x <- char
  if isNewLine x
    then return []
    else (x:) <$> line

exact :: Char -> StrParser Char
exact c = filterP (== c) ("not " ++ [c]) char

oneOfChars :: [Char] -> StrParser Char
oneOfChars chars = foldr (<|>) (failParse $ "not any of " ++ chars) $ map exact chars

upper :: StrParser Char
upper = filterP isAsciiUpper "not one of A-Z" char

lower :: StrParser Char
lower = filterP isAsciiLower "not one of a-z" char

digit :: StrParser Char
digit = filterP isDigit "not a digit" char

space :: StrParser ()
space = ignore $ oneOfChars " \n"

spaces :: StrParser ()
spaces = ignore (many space)

filterP :: (a -> Bool) -> String -> StrParser a -> StrParser a
filterP f msg p = do
  res <- p
  guard (f res) <|> failParse msg
  return res

consume :: StrParser a -> StrParser String
consume p = parser $ \st ->
  let (res, st') = runParser p st in
    case res of
      Fail msg -> (Fail msg, st')
      OK _     -> (OK $ collect st st', st')
  where collect st@(StrState _ posX) dest@(StrState _ posY)
          | posX == posY = []
          | otherwise = let (OK c, st') = runParser char st
                        in c : collect st' dest
