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

import           Lib.Parser       (Result(..), failParse)
import qualified Lib.Parser       as Parser
import           Lib.StringParser (StrParser(..), StrState(..), char, exact, space, spaces)
import qualified Lib.StringParser as StringParser
import qualified Lib.Combinator   as Combinator

import           Prolog.Token (Token)
import qualified Prolog.Token as Token

------------------------------------------------------------
-- tokenizers for Prolog
------------------------------------------------------------

tokenize :: String -> (Result [Token], String)
tokenize code = convert $ flip Parser.runParser (StrState code StringParser.beginPos) $ do
  tokens <- many token
  many $ space <|> Combinator.ignore (exact '\n')
  Combinator.neg char <|> Combinator.ignore token -- raise error when there's a string that couldn't be tokenized
  return tokens
    where convert (Fail msg, StrState rest pos) = (Fail (msg ++ show pos), rest)
          convert (OK val, StrState rest _) = (OK val, rest)

token :: StrParser Token
token = spaces >> foldl1 (<|>) tokenRules
  where tokenRules = [period, func, atom, var, num, str, lparen, rparen, lbracket, rbracket, bar,
                      failParse "unknown token"]

func :: StrParser Token
func = do
  Token.Atom name _ <- atom
  exact '(' -- no space between atom and '('
  return $ Token.Func name

atom :: StrParser Token
atom = atomNormal <|> atomSymbols <|> atomQuoted <|> atomOthers <|> failNotAtom
  where 
    atomNormal :: StrParser Token
    atomNormal = do
      let alphabet = StringParser.lower <|> StringParser.upper
      name <- (:) <$> StringParser.lower <*> many (alphabet <|> StringParser.digit <|> exact '_')
      return $ Token.Atom name False

    atomSymbols :: StrParser Token
    atomSymbols = flip Token.Atom False <$> some (StringParser.oneOfChars symbols)
      where symbols = "~@#$^&*-+=\\/.:?<>"

    atomQuoted :: StrParser Token
    atomQuoted = flip Token.Atom True <$> quoteWith '\''

    atomOthers :: StrParser Token
    atomOthers = flip Token.Atom False . return  <$> StringParser.oneOfChars "!,.;"

    failNotAtom :: StrParser Token
    failNotAtom = failParse "not an atom"

var :: StrParser Token
var = (Token.Var <$> varLexer) <|> failParse "not a variable"
  where
    varLexer = (:) <$> varHead <*> varTail
    varHead = StringParser.upper <|> exact '_'
    varTail = do
      let alphabet = StringParser.lower <|> StringParser.upper
      many $ (alphabet <|> StringParser.digit <|> exact '_')

num :: StrParser Token
num = decimal <|> int <|> failParse "not a number"
  where
    int :: StrParser Token
    int = fmap Token.PInt $ (*) <$> sign <*> value
      where
        sign = s <$> Combinator.option (exact '-')
        s Nothing = 1
        s _ = -1

        value = read <$> some StringParser.digit

    decimal :: StrParser Token
    decimal = fmap (Token.PFloat . read) $ StringParser.consume $ do
      int -- intPart
      exact '.' 
      some StringParser.digit -- fracPart
      Combinator.option (StringParser.oneOfChars "eE" >> int) -- exponent

str :: StrParser Token
str = Token.Str <$> quoteWith '"'

lparen :: StrParser Token
lparen = (exact '(' >> return Token.LParen) <|> failParse "not a left parenthesis"

rparen :: StrParser Token
rparen = (exact ')' >> return Token.RParen) <|> failParse "not a right parenthesis"

lbracket :: StrParser Token
lbracket = (exact '[' >> return Token.LBracket) <|> failParse "not a left bracket"

rbracket :: StrParser Token
rbracket = (exact ']' >> return Token.RBracket) <|> failParse "not a right bracket"

period :: StrParser Token
period = do
  many delim >> exact '.' >> (Combinator.neg char <|> Combinator.ignore (some delim))
  return Token.Period
  where delim = space <|> Combinator.ignore (exact '\n')

bar :: StrParser Token
bar = exact '|' >> return Token.Bar

------------------------------------------------------------
-- utility functions
-------------------------------------------------------------

quoteWith :: Char -> StrParser String
quoteWith q = do
  exact q -- begin quote
  body <- many $ Combinator.except (StringParser.oneOfChars (q:"\\")) char <|> escSeq
  exact q -- end quote
  return body
  where
    escSeq = do
      exact '\\'
      ch <- char
      guard (ch `elem` "abfnrtv'\"\\`") <|> failParse ("unknown character \\" ++ [ch])
      return $ read ("'\\" ++ [ch] ++ "'")
