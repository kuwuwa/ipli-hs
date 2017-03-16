module Prolog (
    module Lib.Parser
  , tokenize, parse
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State

import Lib.Parser
import Lib.Combinator
import Lib.StringParser
import Prolog.AstNode
import Prolog.Operator (initOpData)
import Prolog.Parser (topLevel)
import Prolog.Token
import Prolog.Tokenizer

tokenize :: String -> (Result [Token], String)
tokenize code = convert $ flip runParser (StrState code $ Pos beginNum beginNum) $ do
    tokens <- many token
    many $ space <|> (oneOfChars " \n" >> return ())
    except char (return "ok") <|> failParse "there is unknown token"
    return tokens
  where token = foldl1 (<|>) $ map (spaces >>) tokenRules
        tokenRules = [atom, var, num, str, lparen, rparen, lbracket, rbracket, failParse "unknown token"]
        convert (Fail msg, StrState rest pos) = (Fail (msg ++ show pos), rest)
        convert (OK val, StrState rest pos) = (OK val, rest)

parse :: [Token] -> ([AstNode], [Token])
parse tokenSeq =
  let ((OK asts, rest), _) = runState (runParserT (many topLevel) tokenSeq) initOpData
  in (asts, rest)
