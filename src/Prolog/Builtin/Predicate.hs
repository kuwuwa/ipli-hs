module Prolog.Builtin.Predicate (
  builtinPredicates,
  ioPredicates,
  ) where

import           Control.Applicative
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Control.Monad.IO.Class

import qualified Data.Map as Map

import           Prolog.Node     (Node)
import qualified Prolog.Node     as Node
import           Prolog.Prover   (Predicate, unify, call)
import qualified Prolog.Prover   as Prover
import           Prolog.Database (Database)
import qualified Prolog.Database as Database
import           Prolog.Operator (Operator(Operator))
import qualified Prolog.Operator as Operator

import           Lib.Backtrack (BacktrackT(..), ok)
import qualified Lib.Backtrack as Backtrack

import           System.Exit (exitSuccess)

------------------------------------------------------------

builtinPredicates :: Monad m => Prover.PredDatabase r m
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

ioPredicates :: MonadIO m => Prover.PredDatabase r m
ioPredicates = Map.fromList [
    (("write",  1), write)
  , (("halt",   0), halt)
  ]

------------------------------------------------------------

-- ("true", 0)
true :: Monad m => Predicate r m
true _ = ok

-- ("fail", 0)
fail' :: Monad m => Predicate r m
fail' _ = Backtrack.failWith "fail"

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
  let p = (unify lhs rhs >> Backtrack.fatalWith "neq") <|> ok
  res <- runBacktrackT p k
  return $ case res of
    Backtrack.Fatal "neq" -> Backtrack.Fail $ show lhs ++ " and " ++ show rhs ++ " can be unified"
    _           -> res

-- ("var", 1)
var :: Monad m => Predicate r m
var [term] = do
  case term of
    Node.Var _ -> ok
    _     -> Prover.typeMismatch "variable" term

-- ("nonvar", 1)
nonvar :: Monad m => Predicate r m
nonvar args = BacktrackT $ \k -> do
  let p = (var args >> Backtrack.fatalWith "unexpected var") <|> ok
  res <- runBacktrackT p k
  return $ case res of
    Backtrack.Fatal msg -> Backtrack.Fail msg
    _         -> res

-- ("atom", 1)
atom :: Monad m => Predicate r m
atom [term] = do
  case term of
    Node.Atom _ -> ok
    _      -> Prover.typeMismatch "atom" term

-- ("number", 1)
number :: Monad m => Predicate r m
number [term] = do
  case term of
    Node.PInt _   -> ok
    Node.PFloat _ -> ok
    _        -> Prover.typeMismatch "number" term

-- ("integer", 1)
integer :: Monad m => Predicate r m
integer [term] = do
  case term of
    Node.PInt _ -> ok
    _      -> Prover.typeMismatch "integer" term

-- ("float", 1)
float :: Monad m => Predicate r m
float [term] = do
  case term of
    Node.PFloat _ -> ok
    _        -> Prover.typeMismatch "float" term

-- ("compound", 1)
compound :: Monad m => Predicate r m
compound [term] = do
  case term of
    Node.Func _ _ -> ok
    _        -> Prover.typeMismatch "compound" term

------------------------------

-- ("==", 2)
eq :: Monad m => Predicate r m
eq [lhs, rhs] = if lhs == rhs then ok else Backtrack.failWith "eq"

-- ("=/=", 2)
neq :: Monad m => Predicate r m
neq [lhs, rhs] = if lhs /= rhs then ok else Backtrack.failWith "neq"

------------------------------

-- ("is", 2)
is :: Monad m => Predicate r m
is [lhs, rhs] = Prover.calc rhs >>= unify lhs

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
  Prover.assertNumber lhs >> Prover.assertNumber rhs
  res <- do
    case (lhs, rhs) of
      (Node.PInt lv,   Node.PInt rv)   -> return $ lv             `cmp` rv
      (Node.PFloat lv, Node.PFloat rv) -> return $ lv             `cmp` rv
      (Node.PInt lv,   Node.PFloat rv) -> return $ fromInteger lv `cmp` rv
      (Node.PFloat lv, Node.PInt rv)   -> return $ lv             `cmp` fromInteger rv
  if res then ok
  else do
    [lhsStr, rhsStr] <- lift $ mapM Prover.unparse [lhs, rhs]
    Backtrack.failWith $ "`" ++ lhsStr ++ " " ++ cmpSymbol ++ " " ++ rhsStr ++ "` does not hold"

------------------------------

asserta :: Monad m => Predicate r m
asserta [term] = do
  res <- lift . Prover.liftDB $ Database.prependClause term
  case res of
    Left msg -> Backtrack.fatalWith msg
    Right _  -> ok

assertz :: Monad m => Predicate r m
assertz [term] = do
  res <- lift . Prover.liftDB $ Database.appendClause term
  case res of
    Left msg -> Backtrack.fatalWith msg
    Right _  -> ok

------------------------------

op :: Monad m => Predicate r m
op [a, b, c] = do
  Node.PInt prec  <- Prover.assertPInt a
  let precInt = fromInteger prec
  Node.Atom typ  <- Prover.assertAtom b
  Node.Atom name <- Prover.assertAtom c
  case Operator.readOpType typ of
    Nothing     -> Backtrack.fatalWith "invalid operator specifier"
    Just opType -> lift . Prover.liftOpData $ modify' (Operator.addOp $ Operator name precInt opType)

currentOp :: Monad m => Predicate r m
currentOp [a, b, c] = do
  ops <- map fmt <$> (lift . Prover.liftOpData $ gets Operator.opers)
  foldr1 (<|>) $ map u ops
  where fmt (Operator name prec opType) =
          (Node.Atom name, Node.PInt (fromIntegral prec), Node.Atom (show opType))
        u (name, prec, opType) = unify a prec >> unify b opType >> unify c name
  
------------------------------

cut' :: Monad m => Predicate r m
cut' [] = Backtrack.cut

-- ("\\+", 1)
neg :: Monad m => Predicate r m
neg [term] = BacktrackT $ \k -> do
  res <- runBacktrackT (call term) k
  case res of
    Backtrack.OK _   -> Prover.unparse term >>= return . Backtrack.Fail . (++ " was achieved")
    Backtrack.Fail _ -> runBacktrackT ok k
    fatal            -> return fatal

-- ("once", 1)
once :: Monad m => Predicate r m
once [goal] = call goal >> Backtrack.cut

-- ("repeat", 0)
repeat' :: Monad m => Predicate r m
repeat' [] = ok <|> repeat' []

------------------------------

-- ("atom_length",  2)
atomLength :: Monad m => Predicate r m
atomLength [a0, a1] = do
  Node.Atom a <- Prover.assertAtom a0
  unify a1 (Node.PInt $ fromIntegral (length a))

-- ("atom_concat",  3)
atomConcat :: Monad m => Predicate r m
atomConcat [a0, a1, a2] = do
  case (a0, a1, a2) of
    (Node.Atom l0, Node.Atom l1, _) -> unify (Node.Atom $ l0 ++ l1) a2
    (_, _, Node.Atom l2) -> foldr1 (<|>) $ map f (allSplit l2)
    _                     -> Backtrack.fatalWith "[atom_concat] invalid arguments"
  where f (s0, s1) = unify a0 (Node.Atom s0) >> unify a1 (Node.Atom s1)

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
write [term] = return term >>= lift . Prover.unparse >>= liftIO . putStrLn

-- ("halt", 0)
halt :: MonadIO m => Predicate r m
halt [] = liftIO exitSuccess
