module Lib.Combinator where

import Control.Applicative

import Lib.Parser
import Lib.StringParser

option :: Monad m => ParserT s m a -> ParserT s m (Maybe a)
option p = parserT $ \st -> do
  (res, st') <- runParserT p st
  case res of
    Fail _ -> return (OK Nothing, st)
    OK v   -> return (OK (Just v), st')

except :: Monad m => ParserT s m a -> ParserT s m b -> ParserT s m b
except p q = parserT $ \st -> do
  (res, _) <- runParserT p st
  case res of 
    OK _ -> return (Fail "except", st)
    _    -> runParserT q st
