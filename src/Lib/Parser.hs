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

import           Lib.Result (Result(..))

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.Functor.Identity

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
  {-# INLINE fmap #-}

instance Monad m => Applicative (ParserT s m) where
  pure x = parserT $ \st -> return (OK x, st)
  {-# INLINE pure #-}
  x <*> y = parserT $ \st -> do
    (v, st') <- runParserT x st
    case v of
      Fail msg -> return (Fail msg, st)
      OK f -> runParserT (f <$> y) st'
  {-# INLINE (<*>) #-}

instance Monad m => Alternative (ParserT s m) where
  empty = failParser

  p <|> q = parserT $ \st -> do
    (v, st') <- runParserT p st
    case v of
      Fail _ -> runParserT q st
      _      -> return (v, st')
  {-# INLINE (<|>) #-}

  many p = parserT $ \st -> do
    (v, st') <- runParserT p st
    case v of
      Fail msg -> return (OK [], st)
      OK v -> runParserT ((v:) <$> many p) st'
  {-# INLINE many #-}

  some p = fmap (:) p <*> many p
  {-# INLINE some #-}

instance Monad m => Monad (ParserT s m) where
  return = pure
  {-# INLINE return #-}

  x >>= f = parserT $ \st -> do
    (y, st') <- runParserT x st
    case y of
      Fail msg -> return (Fail msg, st')
      OK v     -> runParserT (f v) st'
  {-# INLINE (>>=) #-}

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
