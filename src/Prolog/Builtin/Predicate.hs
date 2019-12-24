module Prolog.Builtin.Predicate (
  builtinPredicates,
  ioPredicates,
  ) where

import           Control.Applicative
import           Control.Monad 
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Control.Monad.IO.Class

import qualified Data.Map.Strict as Map

import           Prolog.Node     (Node)
import qualified Prolog.Node     as Node
import           Prolog.Prover   (ProverT, Predicate, unify, call)
import qualified Prolog.Prover   as Prover
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
  , (("nl",     0), nl)
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
callp = arity1 >=> call

-- (",", 2)
andp :: Monad m => Predicate r m
andp args = do
  (lhs, rhs) <- arity2 args
  call lhs >> call rhs

-- (";", 2)
orp :: Monad m => Predicate r m
orp args = do
  (lhs, rhs) <- arity2 args
  call lhs <|> call rhs

-- ("=", 2)
unify' :: Monad m => Predicate r m
unify' = arity2 >=> uncurry unify

-- ("\=", 2)
failIfUnified :: Monad m => Predicate r m
failIfUnified args = do
  (lhs, rhs) <- arity2 args
  BacktrackT $ \k -> do
    let p = (unify lhs rhs >> Backtrack.fatalWith "neq") <|> ok
    res <- runBacktrackT p k
    return $ case res of
      Backtrack.Fatal "neq" ->
        Backtrack.Fail $ show lhs ++ " and " ++ show rhs ++ " can be unified"
      _           -> res

-- ("var", 1)
var :: Monad m => Predicate r m
var args = do
  term <- arity1 args
  brVar term ok (Prover.typeMismatch "variable" term)

-- ("nonvar", 1)
nonvar :: Monad m => Predicate r m
nonvar args = do
  term <- arity1 args
  brVar term (Backtrack.failWith "unexpected var") ok

brVar :: Monad m => Node -> ProverT r m a -> ProverT r m a -> ProverT r m a
brVar term k1 k2 =
  case term of
    Node.Var _ -> k1
    _ -> k2

-- ("atom", 1)
atom :: Monad m => Predicate r m
atom args = do
  term <- arity1 args
  case term of
    Node.Atom _ -> ok
    _      -> Prover.typeMismatch "atom" term

-- ("number", 1)
number :: Monad m => Predicate r m
number = arity1 >=> f
  where
    f (Node.PInt _) = ok
    f (Node.PFloat _) = ok
    f t = Prover.typeMismatch "number" t

-- ("integer", 1)
integer :: Monad m => Predicate r m
integer = arity1 >=> f
  where
    f (Node.PInt _) = ok
    f t = Prover.typeMismatch "integer" t

-- ("float", 1)
float :: Monad m => Predicate r m
float = arity1 >=> f
  where
    f (Node.PFloat _) = ok
    f t = Prover.typeMismatch "float" t

-- ("compound", 1)
compound :: Monad m => Predicate r m
compound = arity1 >=> f
  where
    f (Node.Func _ _) = ok
    f t = Prover.typeMismatch "compound" t

------------------------------

-- ("==", 2)
eq :: Monad m => Predicate r m
eq args = do
  (lhs, rhs) <- arity2 args
  if lhs == rhs then ok else Backtrack.failWith "eq"

-- ("=/=", 2)
neq :: Monad m => Predicate r m
neq args = do
  (lhs, rhs) <- arity2 args
  if lhs /= rhs then ok else Backtrack.failWith "neq"

------------------------------

-- ("is", 2)
is :: Monad m => Predicate r m
is args = do
  (lhs, rhs) <- arity2 args
  Prover.calc rhs >>= unify lhs

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
compareNum cmpSymbol cmp args = do
  (lhs, rhs) <- arity2 args
  Prover.assertNumber lhs >> Prover.assertNumber rhs
  res <- do
    case (lhs, rhs) of
      (Node.PInt lv,   Node.PInt rv)   -> return $ lv             `cmp` rv
      (Node.PFloat lv, Node.PFloat rv) -> return $ lv             `cmp` rv
      (Node.PInt lv,   Node.PFloat rv) -> return $ fromInteger lv `cmp` rv
      (Node.PFloat lv, Node.PInt rv)   -> return $ lv             `cmp` fromInteger rv
  if res then ok
  else do
    lhsLit <- lift $ Prover.unparse lhs
    rhsLit <- lift $ Prover.unparse rhs
    Backtrack.failWith $ "`" ++ lhsLit ++ " " ++ cmpSymbol ++ " " ++ rhsLit ++ "` does not hold"

------------------------------

asserta :: Monad m => Predicate r m
asserta args = do
  res <- lift . Prover.liftDB . Database.prependClause =<< arity1 args
  case res of
    Left msg -> Backtrack.fatalWith msg
    Right _  -> ok

assertz :: Monad m => Predicate r m
assertz args = do
  res <- lift . Prover.liftDB . Database.appendClause =<< arity1 args
  case res of
    Left msg -> Backtrack.fatalWith msg
    Right _  -> ok

------------------------------

op :: Monad m => Predicate r m
op args = do
  (a, b, c) <- arity3 args
  Node.PInt prec <- Prover.assertPInt a
  let precInt = fromInteger prec
  Node.Atom typ  <- Prover.assertAtom b
  Node.Atom name <- Prover.assertAtom c
  case Operator.readOpType typ of
    Nothing     -> Backtrack.fatalWith "invalid operator specifier"
    Just opType -> lift . Prover.liftOpData $ modify' (Operator.addOp $ Operator name precInt opType)

currentOp :: Monad m => Predicate r m
currentOp args = do
  (a, b, c) <- arity3 args
  let u (name, prec, opType) = unify a prec >> unify b opType >> unify c name
  ops <- map fmt <$> (lift . Prover.liftOpData $ gets Operator.opers)
  foldr1 (<|>) $ map u ops
  where fmt (Operator name prec opType) =
          (Node.Atom name, Node.PInt (fromIntegral prec), Node.Atom (show opType))
  
------------------------------

cut' :: Monad m => Predicate r m
cut' _ = Backtrack.cut

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
repeat' _ = ok <|> repeat' []

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

-- ("nl", 0)
nl :: MonadIO m => Predicate r m
nl [] = liftIO $ putStrLn ""

------------------------------

arity0 :: Monad m => [a] -> ProverT r m ()
arity0 xs =
  case xs of
    [] -> return ()
    _  -> Backtrack.fatalWith "internal error"

arity1 :: Monad m => [a] -> ProverT r m a
arity1 xs =
  case xs of
    [x] -> return x
    _  -> Backtrack.fatalWith "internal error"

arity2 :: Monad m => [a] -> ProverT r m (a, a)
arity2 xs =
  case xs of
    [x, y] -> return (x, y)
    _  -> Backtrack.fatalWith "internal error"

arity3 :: Monad m => [a] -> ProverT r m (a, a, a)
arity3 xs =
  case xs of
    [x, y, z] -> return (x, y, z)
    _  -> Backtrack.fatalWith "internal error"
