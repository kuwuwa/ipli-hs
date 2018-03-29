module Lib.Result (
  Result(..)
  ) where

type Msg = String

data Result o = Fail Msg | OK o

instance (Eq o) => Eq (Result o) where
  Fail a == Fail b = a == b
  OK a   == OK b   = a == b
  _      == _      = False

instance (Show o) => Show (Result o) where
  show (Fail msg) = "Fail " ++ show msg
  show (OK o)     = "OK " ++ show o

instance Functor Result where
  fmap f (Fail msg) = Fail msg
  fmap f (OK v)     = OK (f v)
