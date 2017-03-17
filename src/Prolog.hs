module Prolog (
    module Prolog.Parser
  , module Prolog.Tokenizer
  , tokenize
  , parse
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State

import Data.Map (Map)
import qualified Data.Map as Map

import Lib.Parser
import Lib.Combinator
import Lib.StringParser
import Prolog.AstNode (AstNode)
import Prolog.Operator (initOpData)
import Prolog.Token (Token)
import Prolog.Parser (TokenStream(..), topLevel)
import Prolog.Tokenizer

tokenize :: String -> (Result [Token], String)
tokenize code = convert $ flip runParser (StrState code beginPos) $ do
    tokens <- many token
    many $ space <|> (oneOfChars " \n" >> return ())
    except char (return "ok") <|> failParse "there is unknown token"
    return tokens
  where token = foldl1 (<|>) $ map (spaces >>) tokenRules
        tokenRules = [period, atom, var, num, str, lparen, rparen, lbracket, rbracket, bar, failParse "unknown token"]
        convert (Fail msg, StrState rest pos) = (Fail (msg ++ show pos), rest)
        convert (OK val, StrState rest pos) = (OK val, rest)

parse :: [Token] -> ([AstNode], [Token])
parse tokenSeq =
  let (((OK asts, TokenStream ind rest), _), _) =
        runState (runStateT (runParserT (many topLevel) (TokenStream 0 tokenSeq)) initOpData) Map.empty
  in (asts, rest)
