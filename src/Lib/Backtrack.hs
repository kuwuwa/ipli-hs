module Lib.Backtrack (
  Result(..),
  BacktrackT(..),
  failWith,
  fatalWith,
  ok,
  cut,
  defer,
  ) where

import           Control.Applicative
import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class

type Msg = String

data Result a = Fatal Msg | Fail Msg | Cut | OK a

instance Show a => Show (Result a) where
  show (Fatal msg) = "Result{Fatal: " ++ msg ++ "}"
  show Cut         = "Result{Cut}"
  show (Fail msg)  = "Result{Fail: " ++ msg ++ "}"
  show (OK v)      = "Result{OK: " ++ show v ++ "}"

instance Eq a => Eq (Result a) where
  Fatal a == Fatal b = a == b
  Fail a  == Fail b  = a == b
  Cut     == Cut     = True
  OK a    == OK b    = a == b
  _       == _       = False

------------------------------------------------------------

newtype BacktrackT r m a = BacktrackT {
  runBacktrackT :: (a -> m (Result r)) -> m (Result r)
}

instance Functor (BacktrackT r m) where
  fmap f m = BacktrackT $ \k -> runBacktrackT m (k . f)

instance Applicative (BacktrackT r m) where
  {-# INLINE pure #-}
  pure x = BacktrackT ($ x)

  {-# INLINE (<*>) #-}
  x <*> y = BacktrackT $ \k -> runBacktrackT x $ \f -> runBacktrackT y (k . f)

instance Monad m => Alternative (BacktrackT r m) where
  {-# INLINE empty #-}
  empty = BacktrackT $ \_ -> return (Fail "empty")

  {-# INLINE (<|>) #-}
  x <|> y = BacktrackT $ \k -> do
    v <- runBacktrackT x k
    case v of
      OK _      -> return v
      Fail _    -> runBacktrackT y k
      Cut       -> return Cut
      Fatal msg -> return $ Fatal msg

instance Monad (BacktrackT r m) where
  {-# INLINE return #-}
  return x = BacktrackT ($ x)

  {-# INLINE (>>) #-}
  x >> y = BacktrackT $ \k -> runBacktrackT x $ \_ -> runBacktrackT y k
  {-# INLINE (>>=) #-}
  x >>= f = BacktrackT $ \k -> runBacktrackT x $ \v -> runBacktrackT (f v) k

instance MonadTrans (BacktrackT r) where
  {-# INLINE lift #-}
  lift m = BacktrackT (m >>=)

instance MonadIO m => MonadIO (BacktrackT r m) where
  {-# INLINE liftIO #-}
  liftIO = lift . liftIO

------------------------------------------------------------

{-# INLINE ok #-}
ok :: Monad m => BacktrackT r m ()
ok = return ()

{-# INLINE failWith #-}
failWith :: Monad m => String -> BacktrackT r m a
failWith msg = BacktrackT $ \_ -> return (Fail msg)

{-# INLINE fatalWith #-}
fatalWith :: Monad m => String -> BacktrackT r m a
fatalWith msg = BacktrackT $ \_ -> return (Fatal msg)

{-# INLINE cut #-}
cut :: Monad m => BacktrackT r m ()
cut = BacktrackT $ \k -> do
  res <- k ()
  return $ case res of
    OK v   -> OK v
    Fail _ -> Cut
    _      -> res

{-# INLINE defer #-}
defer :: Monad m => BacktrackT r m b -> BacktrackT r m ()
defer p = BacktrackT $ \k -> do
  res <- k ()
  runBacktrackT p $ \_ -> return res
