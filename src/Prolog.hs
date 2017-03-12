module Prolog ( tokenize ) where

import Control.Applicative
import Control.Monad

import Lib.Parser
import Lib.Combinator
import Lib.StringParser
import Prolog.Token
import Prolog.Tokenizer

tokenize :: String -> Result [Token]
tokenize code = fst . flip runParser (StrState code $ Pos beginNum beginNum)  $ do
    tokens <- many token
    many $ space <|> (oneOfChars " \n" >> return ())
    except char (return "ok") <|> failParse "there is unknown token"
    return tokens
  where token = foldl1 (<|>) $ map (spaces >>) tokenRules
        tokenRules = [atom, var, num, str, lparen, rparen, lbracket, rbracket, fail]
        fail = failParse "unknown token"
