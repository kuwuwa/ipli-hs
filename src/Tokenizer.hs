module Tokenizer where

import Control.Monad
import Control.Applicative

import Lib.Parser
import Lib.Combinator

----------------------------------------
-- Tokenizer
----------------------------------------

import Token

tokenRules :: [Parser Token]
tokenRules = [atom, var, num, str, lparen, rparen, lbracket, rbracket]

-- data Token = Atom String
--            | Var String
--            | Num Integer
--            | Str String
--            | LParen
--            | RParen
--            | LBracket
--            | RBracket

atom :: Parser Token
atom = atomNormal <|> atomSymbols <|> atomOthers
  where 
    atomNormal :: Parser Token
    atomNormal = do
      id <- consumeBy $ lower >> many (lower <|> upper <|> digit <|> exact '_')
      return $ Atom id

    atomSymbols :: Parser Token
    atomSymbols = do
      id <- some $ oneOf symbols
      return $ Atom id
        where symbols = "~@#$^&*-+=\\/.:?"

    atomOthers :: Parser Token
    atomOthers = do
      id <- oneOf "!,.;"
      return $ Atom [id]

var :: Parser Token
var = do
  varHead <- upper <|> exact '_'
  varTail <- many $ lower <|> upper <|> digit <|> exact '_'
  return $ Var (varHead:varTail)

num :: Parser Token
num = int <|> decimal
  where
    int :: Parser Token
    int = do
      body <- some digit
      return $ Num (read body)

    decimal :: Parser Token
    decimal = do
      body <- consumeBy $ do
        int -- intPart
        exact '.' 
        int -- fracPart
        option $ do -- exponent
          oneOf "eE"
          option $ exact '-'
          int
      return $ Num (read body)

str :: Parser Token
str = do
  exact '"' -- begin double quote
  body <- many $ except (exact '"') charInStr
  exact '"' -- end double quote
  return $ Str body
    where charInStr = except (exact '\\') char <|> escSeq
          escSeq = do
            exact '\\'
            ch <- char
            case ch of 'a' -> return '\a'
                       'b' -> return '\b'
                       'f' -> return '\f'
                       'n' -> return '\n'
                       'r' -> return '\r'
                       't' -> return '\t'
                       'v' -> return '\v'
                       c   -> parseFail $ "unknown character \\" ++ [c]

lparen :: Parser Token
lparen = exact '(' >> return LParen

rparen :: Parser Token
rparen = exact ')' >> return RParen

lbracket :: Parser Token
lbracket = exact '[' >> return LBracket

rbracket :: Parser Token
rbracket = exact ']' >> return RBracket
  
