module Prolog.Tokenizer (
  tokenize,
  token,
  atom,
  var,
  num,
  str,
  lparen,
  rparen,
  lbracket,
  rbracket,
  period,
  bar,
  ) where

import           Control.Monad
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
  many $ space <|> ignore (exact '\n') -- XX: hard-coding
  neg char <|> ignore token -- raise error when there's a string that couldn't be tokenized
  return tokens
    where convert (Fail msg, StrState rest pos) = (Fail (msg ++ show pos), rest)
          convert (OK val, StrState rest _) = (OK val, rest)

token :: StrParser Token
token = spaces >> foldl1 (<|>) tokenRules
  where tokenRules = [period, func, atom, var, num, str, lparen, rparen, lbracket, rbracket, bar,
                      failParse "unknown token"]

func :: StrParser Token
func = do
  Atom name _ <- atom
  exact '(' -- no space between atom and '('
  return $ Func name

atom :: StrParser Token
atom = atomNormal <|> atomSymbols <|> atomQuoted <|> atomOthers <|> failNotAtom
  where 
    atomNormal :: StrParser Token
    atomNormal = do
      name <- (:) <$> lower <*> many (lower <|> upper <|> digit <|> exact '_')
      return $ Atom name False

    atomSymbols :: StrParser Token
    atomSymbols = flip Atom False <$> some (oneOfChars symbols)
      where symbols = "~@#$^&*-+=\\/.:?<>"

    atomQuoted :: StrParser Token
    atomQuoted = flip Atom True <$> quoteWith '\''

    atomOthers :: StrParser Token
    atomOthers = flip Atom False . return  <$> oneOfChars "!,.;"

    failNotAtom :: StrParser Token
    failNotAtom = failParse "not an atom"

var :: StrParser Token
var = (Var <$> varLexer) <|> failParse "not a variable"
  where
    varLexer = (:) <$> varHead <*> varTail
    varHead = upper <|> exact '_'
    varTail = many $ lower <|> upper <|> digit <|> exact '_'

num :: StrParser Token
num = decimal <|> int <|> failParse "not a number"
  where
    int :: StrParser Token
    int = fmap PInt $ (*) <$> sign <*> value
      where
        sign = s <$> option (exact '-')
        s Nothing = 1
        s _ = -1

        value = read <$> some digit

    decimal :: StrParser Token
    decimal = fmap (PFloat . read) $ consume $ do
      int -- intPart
      exact '.' 
      some digit -- fracPart
      option (oneOfChars "eE" >> int) -- exponent

str :: StrParser Token
str = Str <$> quoteWith '"'

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
  many delim >> exact '.' >> (neg char <|> ignore (some delim))
  return Period
  where delim = space <|> ignore (exact '\n')

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
  where
    escSeq = do
      exact '\\'
      ch <- char
      guard (ch `elem` "abfnrtv'\"\\`") <|> failParse ("unknown character \\" ++ [ch])
      return $ read ("'\\" ++ [ch] ++ "'")
