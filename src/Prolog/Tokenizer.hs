module Prolog.Tokenizer (
    tokenize
  , token
  , atom
  , var
  , num
  , str
  , lparen
  , rparen
  , lbracket
  , rbracket
  , period
  , bar
  ) where

import           Control.Applicative

import           Lib.Parser
import           Lib.StringParser
import           Lib.Combinator

import           Prolog.Token

------------------------------------------------------------
-- tokenizers for Prolog
------------------------------------------------------------

tokenize :: String -> (Result [Token], String)
tokenize code = convert $ flip runParser (StrState code beginPos) $ do
  tokens <- many token
  many $ space <|> (exact '\n' >> return ()) -- XX: hard-coding
  -- raise error if there is a string that couldn't be tokenized
  except (char >> return ()) (return ()) <|> (token >> return ())
  return tokens
    where convert (Fail msg, StrState rest pos) = (Fail (msg ++ show pos), rest)
          convert (OK val, StrState rest _) = (OK val, rest)

token :: StrParser Token
token = foldl1 (<|>) $ map (spaces >>) tokenRules
  where tokenRules = [period, func, atom, var, num, str, lparen, rparen, lbracket, rbracket, bar,
                      failParse "unknown token"]

func :: StrParser Token
func = do
  Atom name _ <- atom
  exact '('
  return $ Func name

atom :: StrParser Token
atom = atomNormal <|> atomSymbols <|> atomQuoted <|> atomOthers <|> failNotAtom
  where 
    atomNormal :: StrParser Token
    atomNormal = do
      name <- consume $ lower >> many (lower <|> upper <|> digit <|> exact '_')
      return $ Atom name False

    atomSymbols :: StrParser Token
    atomSymbols = do
      name <- some $ oneOfChars symbols
      return $ Atom name False
        where symbols = "~@#$^&*-+=\\/.:?<>"

    atomQuoted :: StrParser Token
    atomQuoted = do
      body <- quoteWith '\''
      return $ Atom body True

    atomOthers :: StrParser Token
    atomOthers = do
      name <- oneOfChars "!,.;"
      return $ Atom [name] False

    failNotAtom :: StrParser Token
    failNotAtom = failParse "not an atom"

var :: StrParser Token
var = varLexer <|> failParse "not a variable"
  where varLexer = do
          varHead <- upper <|> exact '_'
          varTail <- many $ lower <|> upper <|> digit <|> exact '_'
          return (Var $ varHead:varTail)

num :: StrParser Token
num = decimal <|> int <|> failParse "not a number"
  where
    int :: StrParser Token
    int = do
      sign <- s <$> option (exact '-')
      value <- read <$> some digit
      return $ PInt (sign * value)
        where s Nothing = 1
              s _ = -1

    decimal :: StrParser Token
    decimal = do
      value <- consume $ do
        int -- intPart
        exact '.' 
        some digit -- fracPart
        option $ do -- exponent
          oneOfChars "eE"
          int
      return $ PFloat (read value)

str :: StrParser Token
str = do
  body <- quoteWith '"'
  return $ Str body

lparen :: StrParser Token
lparen = (exact '(' >> return LParen) <|> failParse "not a left parenthesis"

rparen :: StrParser Token
rparen = (exact ')' >> return RParen) <|> failParse "not a right parenthesis"

lbracket :: StrParser Token
lbracket = (exact '[' >> return LBracket) <|> failParse "not a left bracket"

rbracket :: StrParser Token
rbracket = (exact ']' >> return RBracket) <|> failParse "not a right bracket"

period :: StrParser Token
period = do
  many delim >> exact '.' >> (except char (return ()) <|> (some delim >> return ()))
  return Period
  where delim = (space <|> (exact '\n' >> return ())) >> return ()

bar :: StrParser Token
bar = exact '|' >> return Bar

------------------------------------------------------------
-- utility functions
-------------------------------------------------------------

quoteWith :: Char -> StrParser String
quoteWith q = do
  exact q -- begin quote
  body <- many $ except (oneOfChars (q:"\\")) char <|> escSeq
  exact q -- end quote
  return body
    where escSeq = do
            exact '\\'
            ch <- char
            if ch `elem` "abfnrtv'\"\\`" then return $ read ("'\\" ++ [ch] ++ "'")
            else failParse $ "unknown character \\" ++ [ch]
