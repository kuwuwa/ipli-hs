module Prolog.Environment (
    Environment(..)
  , EnvT
  , liftDB
  , liftProcDB
  , liftOpData
  ) where

import Control.Monad.Trans.State

import Prolog.Database (Database)
import Prolog.Procedure
import Prolog.Operator

------------------------------------------------------------

data Environment = Environment { database :: Database, procDatabase :: ProcDatabase, opData :: OpData }

instance Show Environment where
  show env = show (database env)

type EnvT = StateT Environment

liftDB :: Monad m => StateT Database m o -> EnvT m o
liftDB m = StateT $ \env -> do
  (x, db') <- runStateT m $ database env
  return (x, env { database = db' })

liftProcDB :: Monad m => StateT ProcDatabase m o -> EnvT m o
liftProcDB m = StateT $ \env -> do
  (x, db') <- runStateT m $ procDatabase env
  return (x, env { procDatabase = db' })

liftOpData :: Monad m => StateT OpData m o -> EnvT m o
liftOpData m = StateT $ \env -> do
  (x, opData') <- runStateT m $ opData env
  return (x, env { opData = opData' })
