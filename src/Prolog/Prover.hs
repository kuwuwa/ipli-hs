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
  , call
  , fresh
  , resolve
  , unify
  , assertAtom
  , assertNumber
  , assertPInt
  , assertPFloat
  , assertStr
  , assertNil
  , assertFunc
  , typeOf
  ) where

import           Lib.Backtrack (BacktrackT(..), failWith, fatalWith, defer)

import           Prolog.Database (Database)
import           Prolog.Node     (Node(..))
import           Prolog.Operator (OpData)

import           Control.Applicative ((<|>))

import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT(..), evalStateT, gets, modify)

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
  , varNum :: Int
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

call :: Monad m => Node -> ProverT r m ()
call node = do
  assertCallable node
  let (name, args) = case node of
        Atom n -> (n, [])
        Func p a  -> (p, a)
      arity = length args
  procMaybe <- lift $ gets (Map.lookup (name, arity) . predDatabase)
  entriesMaybe <- lift $ gets (Map.lookup (name, arity) . database)
  case (procMaybe, entriesMaybe) of
    (Just proc, _) -> mapM resolve args >>= proc
    (_, Just entries) -> foldr (<|>) failNoAnswer $ map (exec args) entries
    _ -> fatalWith $ "no such predicate: " ++ name
  where
    exec args p = do
      (fParams, fBody) <- fresh p
      sequence_ $ zipWith unify args fParams
      call $ fBody

    failNoAnswer = failWith "no answer"

resolve :: Monad m => Node -> ProverT r m Node
resolve node = do
  case node of
    Var x -> do
      valMaybe <- liftBindingsP . gets $ Map.lookup x
      if not $ isJust valMaybe then do
        liftBindingsP . modify $ Map.insert x (Var x)
        return node
      else do
        let val = fromJust valMaybe
        case val of
          Var y
            | y == x    -> return val
            | otherwise -> do
              val' <- resolve val
              liftBindingsP . modify $ Map.insert x val'
              return val'
          _ -> return val
    Func name args -> Func name <$> mapM resolve args
    _ -> return node

bind :: Monad m => Node -> Node -> ProverT r m ()
bind x y = do
  x' <- resolve x
  y' <- resolve y
  case (x', y') of
    (Var xv, _)
      | xv == "_" -> return () -- _ should not be bound to any value
      | otherwise -> do
        defer (liftBindingsP $ modify (Map.delete xv))
        case y' of
          Var yv -> defer (liftBindingsP $ modify (Map.delete yv))
          _      -> return ()
        liftBindingsP . modify $ Map.insert xv y'
    (_, Var yv)
      | yv == "_" -> return () -- _ should not be bound to any value
      | otherwise -> do
        defer (liftBindingsP $ modify (Map.delete yv))
        case x' of
          Var xv -> defer (liftBindingsP $ modify (Map.delete xv))
          _      -> return ()
        liftBindingsP . modify $ Map.insert yv x'
    _ -> failWith "can't bind two nonvars"

unify :: Monad m => Node -> Node -> ProverT r m ()
unify x y = do
  x' <- resolve x
  y' <- resolve y
  case (x', y') of
    (Func p0 a0, Func p1 a1) -> do
      when (p0 /= p1) $ failWith ("can't unify " ++ show x' ++ " and " ++ show y')
      sequence_ $ zipWith unify a0 a1
    (Var _, _) -> bind x' y'
    (_, Var _) -> bind x' y'
    _
      | x' == y'  -> return ()
      | otherwise -> failWith $ "can't unify " ++ show x' ++ " and " ++ show y'

fresh :: Monad m => ([Node], Node) -> ProverT r m ([Node], Node)
fresh (params, body) = do
  fParams <- mkRenames params
  let renames = map (\(Var v, Var fv) -> (v, fv)) $ filter (isVar . fst) (zip params fParams)
  fBody <- evalStateT (go body) (Map.fromList renames)
  return (fParams, fBody)
  where
    isVar (Var _) = True
    isVar _       = False

    mkRenames = mapM $ \nd -> case nd of
      Var _ -> Var <$> freshVar
      _ -> return nd
          
    go bd = case bd of
      Var v -> do
        wMaybe <- gets $ Map.lookup v
        case wMaybe of
          Just w -> return $ Var w
          Nothing -> do
            newVar <- lift freshVar
            modify $ Map.insert v newVar
            return $ Var newVar
      Func p args -> Func p <$> sequence (map go args)
      _ -> return bd

    freshVar :: Monad m => ProverT r m String
    freshVar = do
      num <- lift (gets varNum)
      let newVar = "_G" ++ show num
      exists <- liftBindingsP $ gets (Map.member newVar)
      lift . modify $ \env -> env { varNum = num + 1 }
      if exists then freshVar else return newVar

------------------------------------------------------------
-- handy functions
------------------------------------------------------------

assertAtom :: Monad m => Node -> ProverT r m ()
assertAtom node = case node of
  Atom _ -> return ()
  _      -> fatalWith $ "atom expected, but got " ++ typeOf node

assertNumber :: Monad m => Node -> ProverT r m ()
assertNumber node = case node of
  PInt _   -> return ()
  PFloat _ -> return ()
  _        -> fatalWith $ "number expected, but got " ++ typeOf node

assertPInt :: Monad m => Node -> ProverT r m ()
assertPInt node = case node of
  PInt _ -> return ()
  _      -> fatalWith $ "integer expected, but got " ++ typeOf node

assertPFloat :: Monad m => Node -> ProverT r m ()
assertPFloat node = case node of
  PFloat _ -> return ()
  _        -> fatalWith $ "float expected, but got " ++ typeOf node

assertStr :: Monad m => Node -> ProverT r m ()
assertStr node = case node of
  Str _ -> return ()
  _     -> fatalWith $ "string expected, but got " ++ typeOf node

assertNil :: Monad m => Node -> ProverT r m ()
assertNil node = case node of
  Nil -> return ()
  _   -> fatalWith $ "nil expected, but got " ++ typeOf node

assertFunc :: Monad m => Node -> ProverT r m ()
assertFunc node = case node of
  Func _ _ -> return ()
  _        -> fatalWith $ "functor expected, but got " ++ typeOf node

assertCallable :: Monad m => Node -> ProverT r m ()
assertCallable node = case node of
  Atom _   -> return ()
  Func _ _ -> return ()
  _        -> fatalWith $ "callable expected, but got " ++ typeOf node

typeOf :: Node -> String
typeOf (Atom _)   = "atom"
typeOf (Var _)    = "variable"
typeOf (PInt _)   = "integer"
typeOf (PFloat _) = "float"
typeOf (Str _)    = "string"
typeOf Nil        = "nil"
typeOf (Func _ _) = "functor"