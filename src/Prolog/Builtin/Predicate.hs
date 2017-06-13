{-# LANGUAGE Rank2Types #-}

module Prolog.Builtin.Predicate (
    builtinPredicates
  ) where

import           Prolog.Node
import           Prolog.Prover

import           Lib.Backtrack

import           Control.Monad
import           Control.Applicative

import qualified Data.Map as Map

builtinPredicates :: Monad m => PredDatabase r m
builtinPredicates = Map.fromList [
    (("true", 0), true)
  , (("fail", 0), fail')

  , ((",",   2), andp)
  , ((";",   2), orp)
  , (("=",   2), unify')
  , (("\\=", 2), failIfUnified)

  , (("var",      1), var)
  , (("nonvar",   1), nonvar)
  , (("atom",     1), atom)
  , (("number",   1), number)
  , (("integer",  1), integer)
  , (("float",    1), float)
  , (("compound", 1), compound)

  , (("==",   2), eq)
  , (("\\==", 2), neq)

  -- , (("is",   2), is)
  , (("=<",   2), lte)
  , (("<",    2), lt)
  , ((">=",   2), gte)
  , ((">",    2), gt)
  , (("=:=",  2), eqNum)
  , (("=\\=", 2), neqNum)

  {--
  , (("asserta", 1), asserta)
  , (("assertz", 1), assertz)
  , (("retract", 1), retract)
  , (("abolish", 1), abolish)
  , (("findall", 3), findall)

  -- IO

  , (("op",         3), op)
  , (("current_op", 3), currentOp)

  , (("once",   1), once)
  , (("repeat", 0), repeat')

  , (("atom_length",  2), atomLength)
  , (("atom_concat",  3), atomConcat)
  , (("sub_atom",     5), subAtom)
  , (("atom_chars",   2), atomChars)
  , (("number_chars", 2), numberChars)
  , (("number_codes", 2), numberCodes)
  , (("number_chars", 2), numberChars)

  , (("halt", 0), halt)
  --}
  ]

-- ("true", 0)
true :: Monad m => Predicate r m
true _ = return ()

-- ("fail", 0)
fail' :: Monad m => Predicate r m
fail' _ = failWith "fail"

-- (",", 2)
andp :: Monad m => Predicate r m
andp [lhs, rhs] = call lhs >> call rhs

-- (";", 2)
orp :: Monad m => Predicate r m
orp [lhs, rhs] = call lhs <|> call rhs

-- ("=", 2)
unify' :: Monad m => Predicate r m
unify' [lhs, rhs] = unify lhs rhs

-- ("\=", 2)
failIfUnified :: Monad m => Predicate r m
failIfUnified [lhs, rhs] = BacktrackT $ \k -> do
  let p = (unify lhs rhs >> fatalWith "neq") <|> return ()
  res <- runBacktrackT p k
  return $ case res of
    Fatal "neq" -> Fail $ show lhs ++ " and " ++ show rhs ++ " can be unified"
    _           -> res

-- ("var", 1)
var :: Monad m => Predicate r m
var [term] = do
  case term of
    Var _ -> return ()
    _     -> failWith $ "variable expected, but got " ++ typeOf term

-- ("nonvar", 1)
nonvar :: Monad m => Predicate r m
nonvar args = BacktrackT $ \k -> do
  let p = (var args >> fatalWith "unexpected var") <|> return ()
  res <- runBacktrackT p k
  return $ case res of
    Fatal msg -> Fail msg
    _         -> res

-- ("atom", 1)
atom :: Monad m => Predicate r m
atom [term] = do
  case term of
    Atom _ -> return ()
    _      -> failWith $ "atom expected, but got " ++ typeOf term

-- ("number", 1)
number :: Monad m => Predicate r m
number [term] = do
  case term of
    PInt _   -> return ()
    PFloat _ -> return ()
    _        -> failWith $ "number expected, but got " ++ typeOf term

-- ("integer", 1)
integer :: Monad m => Predicate r m
integer [term] = do
  case term of
    PInt _ -> return ()
    _      -> failWith $ "integer expected, but got " ++ typeOf term

-- ("float", 1)
float :: Monad m => Predicate r m
float [term] = do
  case term of
    PFloat _ -> return ()
    _        -> failWith $ "float expected, but got" ++ typeOf term

-- ("compound", 1)
compound :: Monad m => Predicate r m
compound [term] = do
  case term of
    Func _ _ -> return ()
    _      -> failWith $ "compound expected, but got " ++ typeOf term

------------------------------

-- ("eq", 2)
eq :: Monad m => Predicate r m
eq [lhs, rhs] = do
  if lhs == rhs then return () else failWith "eq"

-- ("neq", 2)
neq :: Monad m => Predicate r m
neq [lhs, rhs] = do
  if lhs /= rhs then return () else failWith "neq"

------------------------------

-- ("is", 2)
-- is :: Monad m => Predicate r m

-- ("=<", 2)
lte :: Monad m => Predicate r m
lte = compareNum "=<" (<=)

-- ("<", 2)
lt :: Monad m => Predicate r m
lt = compareNum "<" (<)

-- (">=", 2)
gte :: Monad m => Predicate r m
gte = compareNum ">=" (>=)

-- (">", 2)
gt :: Monad m => Predicate r m
gt = compareNum ">" (>)

-- ("=:=", 2)
eqNum :: Monad m => Predicate r m
eqNum = compareNum "=:=" (==)

-- ("=\\=", 2)
neqNum :: Monad m => Predicate r m
neqNum = compareNum "=\\=" (/=)

{-# INLINE compareNum #-}
compareNum :: (Monad m) =>
    String -> (forall a. Ord a => a -> a -> Bool) -> Predicate r m
compareNum cmpSymbol cmp [lhs, rhs] = do
  assertNumber lhs >> assertNumber rhs
  guard $ case (lhs, rhs) of
    (PInt lv,   PInt rv)   -> lv             `cmp` rv
    (PFloat lv, PFloat rv) -> lv             `cmp` rv
    (PInt lv,   PFloat rv) -> fromInteger lv `cmp` rv
    (PFloat lv, PInt rv)   -> lv             `cmp` fromInteger rv
  <|> failWith msg
  where
    msg = "`" ++ show lhs ++ " " ++ cmpSymbol ++ " " ++ show rhs ++ "` does not hold"
