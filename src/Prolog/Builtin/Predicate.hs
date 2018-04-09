{-# LANGUAGE RankNTypes #-}

module Prolog.Builtin.Predicate (
  builtinPredicates,
  ioPredicates,
  ) where

import           Prolog.Node
import           Prolog.Prover
import           Prolog.Database
import           Prolog.Operator (Operator(..), addOp, opers, readOpType)

import           Lib.Backtrack

import           Control.Applicative
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Control.Monad.IO.Class

import qualified Data.Map as Map

import           System.Exit (exitSuccess)

builtinPredicates :: Monad m => PredDatabase r m
builtinPredicates = Map.fromList [
    (("true", 0), true)
  , (("fail", 0), fail')
  , (("call", 1), callp)

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

  , (("is",   2), is)
  , (("=<",   2), lte)
  , (("<",    2), lt)
  , ((">=",   2), gte)
  , ((">",    2), gt)
  , (("=:=",  2), eqNum)
  , (("=\\=", 2), neqNum)

  , (("assert", 1),  assertz)
  , (("asserta", 1), asserta)
  , (("assertz", 1), assertz)

  {--
  , (("retract", 1), retract)
  , (("abolish", 1), abolish)
  , (("findall", 3), findall)
  --}

  , (("op",         3), op)
  , (("current_op", 3), currentOp)

  , (("!", 0), cut')
  , (("\\+",    1), neg)
  , (("once",   1), once)
  , (("repeat", 0), repeat')

  , (("atom_length",  2), atomLength)
  , (("atom_concat",  3), atomConcat)

  {--
  , (("sub_atom",     5), subAtom)
  , (("atom_chars",   2), atomChars)
  , (("number_chars", 2), numberChars)
  , (("number_codes", 2), numberCodes)
  , (("number_chars", 2), numberChars)
  --}
  ]

ioPredicates :: MonadIO m => PredDatabase r m
ioPredicates = Map.fromList [
    (("write",  1), write)
  , (("halt",   0), halt)
  ]

------------------------------

-- ("true", 0)
true :: Monad m => Predicate r m
true _ = ok

-- ("fail", 0)
fail' :: Monad m => Predicate r m
fail' _ = failWith "fail"

-- ("call", 1)
callp :: Monad m => Predicate r m
callp [term] = call term

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
  let p = (unify lhs rhs >> fatalWith "neq") <|> ok
  res <- runBacktrackT p k
  return $ case res of
    Fatal "neq" -> Fail $ show lhs ++ " and " ++ show rhs ++ " can be unified"
    _           -> res

-- ("var", 1)
var :: Monad m => Predicate r m
var [term] = do
  case term of
    Var _ -> ok
    _     -> typeMismatch "variable" term

-- ("nonvar", 1)
nonvar :: Monad m => Predicate r m
nonvar args = BacktrackT $ \k -> do
  let p = (var args >> fatalWith "unexpected var") <|> ok
  res <- runBacktrackT p k
  return $ case res of
    Fatal msg -> Fail msg
    _         -> res

-- ("atom", 1)
atom :: Monad m => Predicate r m
atom [term] = do
  case term of
    Atom _ -> ok
    _      -> typeMismatch "atom" term

-- ("number", 1)
number :: Monad m => Predicate r m
number [term] = do
  case term of
    PInt _   -> ok
    PFloat _ -> ok
    _        -> typeMismatch "number" term

-- ("integer", 1)
integer :: Monad m => Predicate r m
integer [term] = do
  case term of
    PInt _ -> ok
    _      -> typeMismatch "integer" term

-- ("float", 1)
float :: Monad m => Predicate r m
float [term] = do
  case term of
    PFloat _ -> ok
    _        -> typeMismatch "float" term

-- ("compound", 1)
compound :: Monad m => Predicate r m
compound [term] = do
  case term of
    Func _ _ -> ok
    _        -> typeMismatch "compound" term

------------------------------

-- ("==", 2)
eq :: Monad m => Predicate r m
eq [lhs, rhs] = if lhs == rhs then ok else failWith "eq"

-- ("=/=", 2)
neq :: Monad m => Predicate r m
neq [lhs, rhs] = if lhs /= rhs then ok else failWith "neq"

------------------------------

-- ("is", 2)
is :: Monad m => Predicate r m
is [lhs, rhs] = calc rhs >>= unify lhs

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
compareNum :: Monad m => String -> (forall a. Ord a => a -> a -> Bool) -> Predicate r m
compareNum cmpSymbol cmp [lhs, rhs] = do
  assertNumber lhs >> assertNumber rhs
  res <- do
    case (lhs, rhs) of
      (PInt lv,   PInt rv)   -> return $ lv             `cmp` rv
      (PFloat lv, PFloat rv) -> return $ lv             `cmp` rv
      (PInt lv,   PFloat rv) -> return $ fromInteger lv `cmp` rv
      (PFloat lv, PInt rv)   -> return $ lv             `cmp` fromInteger rv
  if res then ok
  else do
    [lhsStr, rhsStr] <- lift $ mapM unparse [lhs, rhs]
    failWith $ "`" ++ lhsStr ++ " " ++ cmpSymbol ++ " " ++ rhsStr ++ "` does not hold"

------------------------------

asserta :: Monad m => Predicate r m
asserta [term] = do
  res <- lift . liftDB $ prependClause term
  case res of
    Left msg -> fatalWith msg
    Right _  -> ok

assertz :: Monad m => Predicate r m
assertz [term] = do
  res <- lift . liftDB $ appendClause term
  case res of
    Left msg -> fatalWith msg
    Right _  -> ok

------------------------------

op :: Monad m => Predicate r m
op [a, b, c] = do
  PInt prec  <- assertPInt a
  let precInt = fromInteger prec
  Atom typ  <- assertAtom b
  Atom name <- assertAtom c
  case readOpType typ of
    Nothing     -> fatalWith "invalid operator specifier"
    Just opType -> lift . liftOpData $ modify' (addOp $ Operator name precInt opType)

currentOp :: Monad m => Predicate r m
currentOp [a, b, c] = do
  opers <- map fmt <$> (lift . liftOpData $ gets opers)
  foldr1 (<|>) $ map u opers
  where fmt (Operator name prec opType) =
          (Atom name, PInt (fromIntegral prec), Atom (show opType))
        u (name, prec, opType) = unify a prec >> unify b opType >> unify c name
  
------------------------------

cut' :: Monad m => Predicate r m
cut' [] = cut

-- ("\\+", 1)
neg :: Monad m => Predicate r m
neg [term] = BacktrackT $ \k -> do
  res <- runBacktrackT (call term) k
  case res of
    OK _   -> unparse term >>= return . Fail . (++ " was achieved")
    Fail _ -> runBacktrackT ok k
    fatal  -> return fatal

-- ("once", 1)
once :: Monad m => Predicate r m
once [goal] = call goal >> cut

-- ("repeat", 0)
repeat' :: Monad m => Predicate r m
repeat' [] = ok <|> repeat' []

------------------------------

-- ("atom_length",  2)
atomLength :: Monad m => Predicate r m
atomLength [a0, a1] = do
  Atom a <- assertAtom a0
  unify a1 (PInt $ fromIntegral (length a))

-- ("atom_concat",  3)
atomConcat :: Monad m => Predicate r m
atomConcat [a0, a1, a2] = do
  case (a0, a1, a2) of
    (Atom l0, Atom l1, _) -> unify (Atom $ l0 ++ l1) a2
    (_, _,       Atom l2) -> foldr1 (<|>) $ map f (allSplit l2)
    _                     -> fatalWith "[atom_concat] invalid arguments"
  where f (s0, s1) = unify a0 (Atom s0) >> unify a1 (Atom s1)

allSplit :: String -> [(String, String)]
allSplit [] = [("", "")]
allSplit (x:xs) = ("", x:xs) : map (\(a, b) -> (x:a, b)) (allSplit xs)

-- ("sub_atom",     5)
-- ("atom_chars",   2)

-- ("number_chars", 2)
-- numberChars :: Monad m => Predicate r m
-- numberChars [num, repr] = do

-- ("number_codes", 2)
-- ("number_chars", 2)

------------------------------

-- ("write", 1)
write :: MonadIO m => Predicate r m
write [term] = return term >>= lift . unparse >>= liftIO . putStrLn

-- ("halt", 0)
halt :: MonadIO m => Predicate r m
halt [] = liftIO exitSuccess
