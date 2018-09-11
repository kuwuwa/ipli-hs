module Prolog.Prover (
  Name,
  Arity,
  Args,
  Bindings,
  Predicate,
  PredDatabase,
  ProverT,
  Environment(..),
  Function,
  liftDB,
  liftOpData,
  bind,
  call,
  -- fresh,
  resolve,
  unify,
  unparse,
  calc,
  assertAtom,
  assertNumber,
  assertPInt,
  assertPFloat,
  assertStr,
  assertNil,
  assertFunc,
  assertCallable,
  typeOf,
  argsNotInstantiated,
  typeMismatch,
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT(..), evalStateT, gets, modify)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Lib.Backtrack (BacktrackT, ok)
import qualified Lib.Backtrack as Backtrack

import           Prolog.Database (Database)
import           Prolog.Node     (Node(Atom, Var, PInt, PFloat, Str, Nil, Func))
import           Prolog.Operator (Operator(..), OpData, OpType(..), fzMap, zfMap, zfzMap)
import qualified Prolog.Parser   as Parser

------------------------------------------------------------

type Name = String
type Arity = Int

type Args = [Node]

type Bindings = Map String Node

type ProverT r m = BacktrackT r (EnvT r m)

type Predicate r m = Args -> ProverT r m ()

type PredDatabase r m = Map (Name, Arity) (Predicate r m)

------------------------------------------------------------

data Environment r m = Environment {
  bindings :: Bindings,
  database :: Database,
  funcDatabase :: FuncDatabase r m,
  predDatabase :: Map (Name, Arity) (Predicate r m),
  varNum :: Int,
  opData :: OpData
}

type EnvT r m = StateT (Environment r m) m

liftBindings :: Monad m => StateT Bindings m o -> EnvT r m o
liftBindings m = StateT $ \env -> do
  (x, bd') <- runStateT m $ bindings env
  return (x, env { bindings = bd' })

liftDB :: Monad m => StateT Database m o -> EnvT r m o
liftDB m = StateT $ \env -> do
  (x, db') <- runStateT m $ database env
  return (x, env { database = db' })

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
        Atom n   -> (n, [])
        Func f a -> (f, a)
      arity = length args
  procMaybe    <- lift $ gets (Map.lookup (name, arity) . predDatabase)
  entriesMaybe <- lift $ gets (Map.lookup (name, arity) . database)
  case (procMaybe, entriesMaybe) of
    (Just proc,    _) -> proc =<< mapM resolve args
    (_, Just entries) -> foldr (<|>) failNoAnswer $ map (exec args) entries
    _                 -> Backtrack.fatalWith $ "Undefined procedure: " ++ name ++ "/" ++ show (length args)
  where
    exec args p = do
      (fParams, fBody) <- fresh p
      zipWithM_ unify args fParams
      call fBody

    failNoAnswer = Backtrack.failWith "no more answer"


resolve :: Monad m => Node -> ProverT r m Node
resolve node = do
  case node of
    Var x -> do
      valMaybe <- liftBindingsP . gets $ Map.lookup x
      case valMaybe of
        Nothing                   -> register x node
        Just val@(Var y) | y /= x -> resolve val >>= register x
        Just val                  -> return val
    Func name args -> Func name <$> mapM resolve args
    _              -> return node
  where 
    register name node = liftBindingsP (modify $ Map.insert name node) >> return node
    

bind :: Monad m => Node -> Node -> ProverT r m ()
bind x y = do
  x' <- resolve x
  y' <- resolve y
  case (x', y') of
    (Var xv,  _     ) -> if xv == "_" then ok else bind' xv y'
    (_,       Var yv) -> if yv == "_" then ok else bind' yv x'
    _                 -> Backtrack.failWith "can't bind two nonvars"
  where
    bind' v term =
      if v == "_" then ok else do
      Backtrack.defer $ liftBindingsP (modify $ Map.delete v)
      case term of
        Var w -> Backtrack.defer $ liftBindingsP (modify $ Map.delete w)
        _     -> ok
      liftBindingsP $ modify (Map.insert v term)


unify :: Monad m => Node -> Node -> ProverT r m ()
unify x y = do
  x' <- resolve x
  y' <- resolve y
  case (x', y') of
    (Func p0 a0, Func p1 a1) -> do
      when (p0 /= p1) $ Backtrack.failWith ("can't unify " ++ show x' ++ " and " ++ show y')
      zipWithM_ unify a0 a1
    (Var _, _) -> bind x' y'
    (_, Var _) -> bind x' y'
    _
      | x' == y'  -> ok
      | otherwise -> Backtrack.failWith $ "can't unify " ++ show x' ++ " and " ++ show y'


fresh :: Monad m => ([Node], Node) -> ProverT r m ([Node], Node)
fresh (params, body) = do
  (fParams, st') <- runStateT (mapM go params) Map.empty
  fBody <- evalStateT (go body) st'
  return (fParams, fBody)
  where
    isVar (Var _) = True
    isVar _       = False

    go bd = case bd of
      Var v | v /= "_" -> do
        wMaybe <- gets $ Map.lookup v
        case wMaybe of
          Just w  -> return $ Var w
          Nothing -> do
            fVar <- lift mkFreshVar
            modify $ Map.insert v fVar
            return $ Var fVar
      Func p args -> Func p <$> mapM go args
      _ -> return bd

    mkFreshVar :: Monad m => ProverT r m String
    mkFreshVar = do
      fVar <- ("_V" ++) <$> show <$> lift (gets varNum) 
      exists <- liftBindingsP $ gets (Map.member fVar)
      lift . modify $ \env -> env { varNum = varNum env + 1 }
      if exists then mkFreshVar else return fVar

----------------------------------------------------------
-- calc
----------------------------------------------------------

type Function r m = Args -> ProverT r m Node

type FuncDatabase r m = Map (Name, Arity) (Function r m)

calc :: Monad m => Node -> ProverT r m Node
calc x =
  case x of
    PInt _   -> return x
    PFloat _ -> return x
    Func name rawArgs -> do
      arithFuncs <- lift $ gets funcDatabase
      case Map.lookup (name, length rawArgs) arithFuncs of
        Nothing -> do
          lit <- lift $ unparse x
          Backtrack.fatalWith $ lit ++ " is not a function"
        Just f -> mapM calc rawArgs >>= f
    Var _ -> argsNotInstantiated
    _ -> Backtrack.fatalWith "arithmetic term expected"

----------------------------------------------------------
-- unparser
----------------------------------------------------------

unparse :: Monad m => Node -> StateT (Environment r m) m String
unparse (Atom a)   = return a -- TODO: quote when necessary
unparse (Var v)    = return v
unparse (PInt i)   = return (show i)
unparse (PFloat f) = return (show f)
unparse (Str s)    = return s
unparse Nil        = return "[]"
unparse _func@(Func "[|]" [_, _]) = unparseList _func
unparse _func@(Func _ _) = unparseFunc Parser.upperPrecLimit _func
  where
    unparseFunc prec func@(Func name [term]) = do
      fzOpMaybe <- liftOpData $ gets (Map.lookup name . fzMap)
      zfOpMaybe <- liftOpData $ gets (Map.lookup name . zfMap)
      case (fzOpMaybe, zfOpMaybe) of
        (Just (Operator _ prec' opType), _) -> do
          let needParen = prec' > prec
              precNext = if opType == Fy then prec' else prec' - 1
          content <- (name ++) <$> (" " ++) <$> unparseFunc precNext term
          return $ if needParen then "(" ++ content ++ ")" else content
        (_, Just (Operator _ prec' opType)) -> do
          let needParen = prec' > prec
              precNext = if opType == Yf then prec' else prec' - 1
          content <- (++ " " ++ name) <$> unparseFunc precNext term 
          return $ if needParen then "(" ++ content ++ ")" else content
        _ -> unparseFuncDefault func

    unparseFunc prec func@(Func name [lhs, rhs]) = do
      if isPair func then unparseList func
      else do
        zfzOpMaybe <- liftOpData $ gets (Map.lookup name . zfzMap)
        case zfzOpMaybe of
          Just (Operator _ prec' opType) -> do
            let needParen = prec' > prec
                precLhs = if opType == Yfx then prec' else prec' - 1
                precRhs = if opType == Xfy then prec' else prec' - 1
            lhsStr <- unparseFunc precLhs lhs
            rhsStr <- unparseFunc precRhs rhs
            let content = lhsStr ++ " " ++ name ++ " " ++ rhsStr
            return $ if needParen then "(" ++ content ++ ")" else content
          _ -> unparseFuncDefault func
    unparseFunc _ func@(Func _ _) = unparseFuncDefault func
    unparseFunc _ term = unparse term

    unparseFuncDefault (Func name args) = do
      -- 1000 is the precedence of comma
      argsStr <- mapM (unparseFunc $ pred 999) args
      return $ name ++ "(" ++ joinList ", " argsStr ++ ")"

    isPair (Func "[|]" [_,_]) = True
    isPair _ = False

    joinList _ [] = ""
    joinList delim (x:xs) = concat $ x : zipWith (++) (repeat delim) xs

unparseList :: Monad m => Node -> StateT (Environment r m) m String
unparseList (Func "[|]" [hd, tl]) = do
  hdStr <- unparse hd
  ("[" ++) <$> (hdStr ++) <$> unparseTail tl
  where
    unparseTail Nil = return "]"
    unparseTail (Func "[|]" [hd', tl']) = do
        hdStr <- unparse hd'
        (", " ++) <$> (hdStr ++) <$> unparseTail tl'
    unparseTail term = do
        termStr <- unparse term
        return $ " | " ++ termStr ++ "]"

------------------------------------------------------------
-- handy functions
------------------------------------------------------------

assertAtom :: Monad m => Node -> ProverT r m Node
assertAtom node = case node of
  Atom _ -> return node
  Var _  -> argsNotInstantiated
  _      -> typeMismatchFatal "atom" node

assertNumber :: Monad m => Node -> ProverT r m Node
assertNumber node = case node of
  PInt _   -> return node
  PFloat _ -> return node
  Var _    -> argsNotInstantiated
  _        -> typeMismatchFatal "number" node

assertPInt :: Monad m => Node -> ProverT r m Node
assertPInt node = case node of
  PInt _ -> return node
  Var _  -> argsNotInstantiated
  _      -> typeMismatchFatal "integer" node

assertPFloat :: Monad m => Node -> ProverT r m Node
assertPFloat node = case node of
  PFloat _ -> return node
  Var _    -> argsNotInstantiated
  _        -> typeMismatchFatal "float" node

assertStr :: Monad m => Node -> ProverT r m Node
assertStr node = case node of
  Str _ -> return node 
  Var _ -> argsNotInstantiated
  _     -> typeMismatchFatal "string" node

assertNil :: Monad m => Node -> ProverT r m Node
assertNil node = case node of
  Nil    -> return node
  Var _  -> argsNotInstantiated
  _      -> typeMismatchFatal "nil" node

assertFunc :: Monad m => Node -> ProverT r m Node
assertFunc node = case node of
  Func _ _ -> return node
  Var _    -> argsNotInstantiated
  _        -> typeMismatchFatal "functor" node

assertCallable :: Monad m => Node -> ProverT r m Node
assertCallable node = case node of
  Atom _   -> return node
  Func _ _ -> return node
  Var _    -> argsNotInstantiated
  _        -> typeMismatchFatal "callable" node

argsNotInstantiated :: Monad m => ProverT r m a
argsNotInstantiated = Backtrack.fatalWith $ "arguments are not sufficiently instantiated"

typeMismatchFatal :: Monad m => String -> Node -> ProverT r m a
typeMismatchFatal expected actual =
  Backtrack.fatalWith $ expected ++ " expected, but got " ++ typeOf actual

typeMismatch :: Monad m => String -> Node -> ProverT r m a
typeMismatch expected actual =
  Backtrack.failWith $ expected ++ " expected, but got " ++ typeOf actual

typeOf :: Node -> String
typeOf (Atom _)   = "atom"
typeOf (Var _)    = "variable"
typeOf (PInt _)   = "integer"
typeOf (PFloat _) = "float"
typeOf (Str _)    = "string"
typeOf Nil        = "nil"
typeOf (Func _ _) = "functor"
