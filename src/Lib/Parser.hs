module Lib.Parser
  ( Result(..)
  , ParserT(..)
  , Parser
  , parserT
  , runParser
  , parser
  , failParser
  , failParse
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.Functor.Identity

type Msg = String

data Result o = Fail Msg | OK o
instance (Eq o) => Eq (Result o) where
  Fail a == Fail b = a == b
  OK a   == OK b   = a == b
  _      == _      = False

instance (Show o) => Show (Result o) where
  show (Fail msg) = "Fail " ++ show msg
  show (OK o) = "OK " ++ show o

instance Functor Result where
  fmap f (Fail msg) = Fail msg
  fmap f (OK v) = OK (f v)

--------------------

data ParserT s m o = ParserT { runParserT :: s -> m (Result o, s)  }

type Parser s o = ParserT s Identity o

parserT :: (s -> m (Result o, s)) -> ParserT s m o
parserT = ParserT

runParser :: Parser s o -> s -> (Result o, s)
runParser p st = runIdentity $ runParserT p st

parser :: (s -> (Result o, s)) -> Parser s o
parser f = parserT (Identity . f)

failParser :: (Monad m) => ParserT s m o
failParser = parserT $ \st -> return (Fail "failParser", st)

instance Monad m => Functor (ParserT s m) where
  fmap f p = parserT $ \st -> do
    res <- runParserT p st
    return $ case res of
               (OK o, st') -> (OK (f o), st')
               (Fail msg, _) -> (Fail msg, st)

instance Monad m => Applicative (ParserT s m) where
  pure x = parserT $ \st -> return (OK x, st)
  x <*> y = parserT $ \st -> do
    (v, st') <- runParserT x st
    case v of
      Fail msg -> return (Fail msg, st)
      OK f -> do
        (w, st'') <- runParserT y st'
        return $ case w of
                   Fail msg -> (Fail msg, st)
                   OK x -> (OK (f x), st'')

instance Monad m => Alternative (ParserT s m) where
  empty = failParser

  p <|> q = parserT $ \st -> do
    (v, st') <- runParserT p st
    case v of
      Fail _ -> runParserT q st
      _      -> return (v, st')

  many p = parserT $ \st -> do
    (v, st') <- runParserT p st
    case v of
      Fail msg -> return (OK [], st)
      OK v -> do
        (OK vs, st'') <- runParserT (many p) st'
        return (OK (v:vs), st'')

  some p = fmap (:) p <*> many p

instance Monad m => Monad (ParserT s m) where
  return = pure

  x >>= f = parserT $ \st -> do
    (y, st') <- runParserT x st
    case y of
      Fail msg -> return (Fail msg, st')
      OK v     -> runParserT (f v) st'

instance Monad m => MonadPlus (ParserT s m) where
  mzero = empty
  mplus = (<|>)

-- data ParserT s m o = ParserT { runParserT :: s -> m (Result o, s) }

instance MonadTrans (ParserT s) where
  -- lift :: (Monad m) => m a -> ParserT s m a
  lift m = parserT $ \st -> do
    v <- m
    return (OK v, st)

failParse :: Monad m => String -> ParserT s m a
failParse msg = parserT $ \st -> return (Fail msg, st)
