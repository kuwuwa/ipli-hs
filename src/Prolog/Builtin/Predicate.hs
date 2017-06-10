module Prolog.Builtin.Predicate (
    builtinPredicates
  ) where

import           Prolog.Database
import           Prolog.Node
import           Prolog.Prover

import           Lib.Backtrack

import           Control.Applicative
import           Control.Monad

import           Data.Map (Map)
import qualified Data.Map as Map

builtinPredicates :: Monad m => PredDatabase r m
builtinPredicates = Map.fromList [
    (("true", 0), true)

  , ((",", 2), andp)
  , ((";", 2), orp)
  , (("=", 2), equals)
  ]

-- ("true", 0)
true :: Monad m => Predicate r m
true [] = return ()

-- (",", 2)
andp :: Monad m => Predicate r m
andp [lhs, rhs] = call lhs >> call rhs

-- (";", 2)
orp :: Monad m => Predicate r m
orp [lhs, rhs] = call lhs <|> call rhs

-- ("=", 2)
equals :: Monad m => Predicate r m
equals [lhs, rhs] = unify lhs rhs
