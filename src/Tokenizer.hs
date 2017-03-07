module Tokenizer ( tokenize ) where

import Control.Monad
import Control.Applicative

import Lib.Parser
import Lib.StringParser
import Lib.Combinator

import Token


tokenize :: Parser [Token]
tokenize = many token
  where token = foldl1 (<|>) $ map (spaces >>) tokenRules
        tokenRules = [atom, var, num, str, lparen, rparen, lbracket, rbracket, fail]
        fail = parseFail "unknown token"


atom :: Parser Token
atom = atomNormal <|> atomSymbols <|> atomQuoted <|> atomOthers
  where 
    atomNormal :: Parser Token
    atomNormal = do
      id <- consume $ lower >> many (lower <|> upper <|> digit <|> exact '_')
      return $ Atom id

    atomSymbols :: Parser Token
    atomSymbols = do
      id <- some $ oneOfChars symbols
      return $ Atom id
        where symbols = "~@#$^&*-+=\\/.:?<>"

    atomQuoted :: Parser Token
    atomQuoted = do
      body <- quotedWith '\''
      return $ Atom body

    atomOthers :: Parser Token
    atomOthers = do
      id <- oneOfChars "!,.;"
      return $ Atom [id]

var :: Parser Token
var = do
  varHead <- upper <|> exact '_'
  varTail <- many $ lower <|> upper <|> digit <|> exact '_'
  return $ Var (varHead:varTail)

num :: Parser Token
num = decimal <|> int
  where
    int :: Parser Token
    int = do
      sign <- s <$> option (exact '-')
      value <- read <$> some digit
      return $ NumI (sign * value)
        where s Nothing = 1
              s _ = -1

    decimal :: Parser Token
    decimal = do
      value <- consume $ do
        int -- intPart
        exact '.' 
        some digit -- fracPart
        option $ do -- exponent
          oneOfChars "eE"
          option $ exact '-'
          int
      return $ NumF (read value)

str :: Parser Token
str = do
  body <- quotedWith '"'
  return $ Str body

lparen :: Parser Token
lparen = exact '(' >> return LParen

rparen :: Parser Token
rparen = exact ')' >> return RParen

lbracket :: Parser Token
lbracket = exact '[' >> return LBracket

rbracket :: Parser Token
rbracket = exact ']' >> return RBracket

--------------------

quotedWith :: Char -> Parser String
quotedWith q = do
  exact q -- begin quote
  body <- many $ except (oneOfChars (q:"\\")) char <|> escSeq
  exact q -- end quote
  return body
    where escSeq = do
            exact '\\'
            ch <- char
            if ch `elem` "abfnrtv'\"\\`" then return $ read ('\\':[ch])
            else parseFail $ "unknown character \\" ++ [ch]
