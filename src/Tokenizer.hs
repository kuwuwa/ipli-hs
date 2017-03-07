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
      id <- consume $ lower >> many (lower <|> upper <|> digit <|> exact '_')
      return $ Atom id

    atomSymbols :: Parser Token
    atomSymbols = do
      id <- some $ oneOf symbols
      return $ Atom id
        where symbols = "~@#$^&*-+=\\/.:?<>"

    atomOthers :: Parser Token
    atomOthers = do
      id <- oneOf "!,.;"
      return $ Atom [id]

    -- TODO: implement atomQuote

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
      body <- some digit
      return $ Num (read body)

    decimal :: Parser Token
    decimal = do
      body <- consume $ do
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
    where charInStr = except (oneOf "\"\\") char <|> escSeq
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
                       '"' -> return '"'
                       c   -> parseFail $ "unknown character \\" ++ [c]
  -- this parser gives correct results, but gives insufficient failure message

lparen :: Parser Token
lparen = exact '(' >> return LParen

rparen :: Parser Token
rparen = exact ')' >> return RParen

lbracket :: Parser Token
lbracket = exact '[' >> return LBracket

rbracket :: Parser Token
rbracket = exact ']' >> return RBracket
