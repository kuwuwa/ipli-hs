module Lib.Combinator where

import           Lib.Parser

option :: Monad m => ParserT s m a -> ParserT s m (Maybe a)
option p = ParserT $ \st -> do
  (res, st') <- runParserT p st
  case res of
    Fail _ -> return (OK Nothing, st)
    OK v   -> return (OK (Just v), st')

except :: Monad m => ParserT s m a -> ParserT s m b -> ParserT s m b
except p q = ParserT $ \st -> do
  (res, _) <- runParserT p st
  case res of 
    OK _ -> return (Fail "except", st)
    _    -> runParserT q st
