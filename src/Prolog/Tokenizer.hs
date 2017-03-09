module Prolog.Tokenizer (
    atom
  , var
  , num
  , str
  , lparen
  , rparen
  , lbracket
  , rbracket
  ) where

import Control.Monad
import Control.Applicative

import Lib.Parser
import Lib.StringParser
import Lib.Combinator

import Prolog.Token

type StringParser o = Parser StrStream o

atom :: StringParser Token
atom = atomNormal <|> atomSymbols <|> atomQuoted <|> atomOthers <|> failNotAtom
  where 
    atomNormal :: StringParser Token
    atomNormal = do
      id <- consume $ lower >> many (lower <|> upper <|> digit <|> exact '_')
      return $ Atom id

    atomSymbols :: StringParser Token
    atomSymbols = do
      id <- some $ oneOfChars symbols
      return $ Atom id
        where symbols = "~@#$^&*-+=\\/.:?<>"

    atomQuoted :: StringParser Token
    atomQuoted = do
      body <- quotedWith '\''
      return $ Atom body

    atomOthers :: StringParser Token
    atomOthers = do
      id <- oneOfChars "!,.;"
      return $ Atom [id]

    failNotAtom :: StringParser Token
    failNotAtom = failParse "not an atom"

var :: StringParser Token
var = varLexer <|> failParse "not a variable"
  where varLexer = do
          varHead <- upper <|> exact '_'
          varTail <- many $ lower <|> upper <|> digit <|> exact '_'
          return (Var $ varHead:varTail)

num :: StringParser Token
num = decimal <|> int <|> failParse "not a number"
  where
    int :: StringParser Token
    int = do
      sign <- s <$> option (exact '-')
      value <- read <$> some digit
      return $ PInt (sign * value)
        where s Nothing = 1
              s _ = -1

    decimal :: StringParser Token
    decimal = do
      value <- consume $ do
        int -- intPart
        exact '.' 
        some digit -- fracPart
        option $ do -- exponent
          oneOfChars "eE"
          option $ exact '-'
          int
      return $ PFloat (read value)

str :: StringParser Token
str = do
  body <- quotedWith '"'
  return $ Str body

lparen :: StringParser Token
lparen = (exact '(' >> return LParen) <|> failParse "not a left parenthesis"

rparen :: StringParser Token
rparen = (exact ')' >> return RParen) <|> failParse "not a right parenthesis"

lbracket :: StringParser Token
lbracket = (exact '[' >> return LBracket) <|> failParse "not a left bracket"

rbracket :: StringParser Token
rbracket = (exact ']' >> return RBracket) <|> failParse "not a right bracket"

--------------------

quotedWith :: Char -> StringParser String
quotedWith q = do
  exact q -- begin quote
  body <- many $ except (oneOfChars (q:"\\")) char <|> escSeq
  exact q -- end quote
  return body
    where escSeq = do
            exact '\\'
            ch <- char
            if ch `elem` "abfnrtv'\"\\`" then return $ read ("'\\" ++ [ch] ++ "'")
            else failParse $ "unknown character \\" ++ [ch]
