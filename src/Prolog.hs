module Prolog ( tokenize ) where

import Control.Applicative
import Control.Monad

import Lib.Parser
import Lib.StringParser
import Prolog.Token
import Prolog.Tokenizer


tokenize :: Parser [Token]
tokenize = many token
  where token = foldl1 (<|>) $ map (spaces >>) tokenRules
        tokenRules = [atom, var, num, str, lparen, rparen, lbracket, rbracket, fail]
        fail = failParse "unknown token"
