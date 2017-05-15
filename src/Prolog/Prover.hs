module Prolog.Prover (
    Name
  , Arity
  , Args
  , Bindings
  , Predicate
  , PredDatabase
  , ProverT
  , Environment(..)
  , liftDB
  , liftPredDB
  , liftOpData
  , bind
  , resolve
  , unify
  ) where

import           Lib.Backtrack (BacktrackT, failWith)

import           Prolog.Database (Database)
import           Prolog.Node     (Node(..))
import           Prolog.Operator (Operator(..), OpData)

import           Control.Applicative ((<|>), empty)

import           Control.Monad
import           Control.Monad.IO.Class    (MonadIO(..))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (State, StateT(..), gets, modify)

import           Data.Maybe (fromJust, isJust)

import           Data.Map (Map)
import qualified Data.Map as Map

------------------------------------------------------------

type Name = String
type Arity = Int

type Args = [Node]

type Bindings = Map String Node

type Predicate r m = Args -> ProverT r m ()

type PredDatabase r m = Map (Name, Arity) (Predicate r m)

type ProverT r m = BacktrackT r (EnvT r m)

------------------------------------------------------------

data Environment r m = Environment {
    bindings :: Bindings
  , database :: Database
  , predDatabase :: Map (Name, Arity) (Predicate r m)
  , opData :: OpData
}

type EnvT r m = StateT (Environment r m) m

liftBindings :: Monad m => StateT Bindings m o -> EnvT r m o
liftBindings m = StateT $ \env -> do
  (x, bindings') <- runStateT m $ bindings env
  return (x, env { bindings = bindings' })

liftDB :: Monad m => StateT Database m o -> EnvT r m o
liftDB m = StateT $ \env -> do
  (x, db') <- runStateT m $ database env
  return (x, env { database = db' })

liftPredDB :: Monad m => StateT (PredDatabase r m) m o -> EnvT r m o
liftPredDB m = StateT $ \env -> do
  (x, db') <- runStateT m $ predDatabase env
  return (x, env { predDatabase = db' })

liftOpData :: Monad m => StateT OpData m o -> EnvT r m o
liftOpData m = StateT $ \env -> do
  (x, opData') <- runStateT m $ opData env
  return (x, env { opData = opData' })

------------------------------------------------------------

liftBindingsP :: Monad m => StateT Bindings m o -> ProverT r m o
liftBindingsP = lift . liftBindings

resolve :: Monad m => Node -> ProverT r m Node
resolve node = do
  case node of
    Var x -> do
      valMaybe <- liftBindingsP . gets $ Map.lookup x
      if not $ isJust valMaybe then do
        bind node node
        return node
      else do
        let val = fromJust valMaybe
        case val of
          Var y
            | y == x    -> return val
            | otherwise -> do
              val' <- resolve val
              bind node val'
              return val'
          _ -> return val
    _ -> return node

bind :: Monad m => Node -> Node -> ProverT r m ()
bind x y = do
  case (x, y) of
    (Var xv, _)
      | xv == "_" -> return () -- _ should not be bound to any value
      | otherwise -> liftBindingsP . modify $ Map.insert xv y
    (_, Var yv)
      | yv == "_" -> return () -- _ should not be bound to any value
      | otherwise -> liftBindingsP . modify $ Map.insert yv x
    _ -> failWith "can't bind two nonvars"

unify :: Monad m => Node -> Node -> ProverT r m ()
unify x y = do
  x' <- resolve x
  y' <- resolve y
  bind x' y' <|> case (x', y') of
    (Func p0 a0, Func p1 a1) -> do
      when (p0 /= p1) $ failWith ("can't unify " ++ show x' ++ " and " ++ show y')
      foldr1 (>>) $ zipWith unify a0 a1
    _
      | x' == y'  -> return ()
      | otherwise -> failWith $ "can't unify " ++ show x' ++ " and " ++ show y'
