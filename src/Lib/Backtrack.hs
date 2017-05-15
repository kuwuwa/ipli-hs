module Lib.Backtrack (
    BResult(..) 
  , BacktrackT(..)
  , failWith
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class

type Msg = String

data BResult a = Fatal Msg | Fail Msg | OK a

instance Show a => Show (BResult a) where
  show (Fatal msg) = "BResult{Fatal: " ++ msg ++ "}"
  show (Fail msg)  = "BResult{Fail: " ++ msg ++ "}"
  show (OK v)      = "BResult{OK: " ++ show v ++ "}"

instance Eq a => Eq (BResult a) where
  Fatal a == Fatal b = a == b
  Fail a  == Fail b  = a == b
  OK a    == OK b    = a == b
  _       == _       = False

------------------------------------------------------------

newtype BacktrackT r m a = BacktrackT {
  runBacktrackT :: (a -> m (BResult r)) -> m (BResult r)
}

instance Functor (BacktrackT r m) where
  fmap f m = BacktrackT $ \k -> runBacktrackT m (k . f)

instance Applicative (BacktrackT r m) where
  pure x = BacktrackT ($ x)

  x <*> y = BacktrackT $ \k -> runBacktrackT x $ \f -> runBacktrackT y (k . f)

instance Monad m => Alternative (BacktrackT r m) where
  empty = BacktrackT $ \_ -> return (Fail "empty")

  x <|> y = BacktrackT $ \k -> runBacktrackT x $ \w -> do
    v <- k w
    case v of
      OK r      -> return $ OK r
      Fail _    -> runBacktrackT y k
      Fatal msg -> return $ Fatal msg

instance Monad (BacktrackT r m) where
  return x = BacktrackT ($ x)

  x >> y = BacktrackT $ \k -> runBacktrackT x $ \_ -> runBacktrackT y k
  x >>= f = BacktrackT $ \k -> runBacktrackT x $ \v -> runBacktrackT (f v) k

instance MonadTrans (BacktrackT r) where
  lift m = BacktrackT (m >>=)

------------------------------------------------------------

failWith :: Monad m => String -> BacktrackT r m a
failWith msg = BacktrackT $ \_ -> return (Fail msg)

cut :: Monad m => BacktrackT r m ()
cut = BacktrackT $ \k -> do
  res <- k ()
  return $ case res of
    OK v      -> OK v
    Fail msg  -> Fatal $ "[cut]" ++ msg
    _         -> res
