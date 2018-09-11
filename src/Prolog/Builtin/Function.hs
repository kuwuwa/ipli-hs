module Prolog.Builtin.Function (
  builtinFuncs
) where

import           Lib.Backtrack

import           Prolog.Node   (Node(PInt, PFloat))
import           Prolog.Prover (ProverT(..), Function, Name, Arity)
import qualified Prolog.Prover as Prover

import           Control.Monad
import           Control.Monad.Trans.Class (lift)

import qualified Data.Bits as Bits
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)


builtinFuncs :: Monad m => Map (Name, Arity) (Function r m)
builtinFuncs = Map.fromList [
    (("+", 1),    plus)
  , (("-", 1),    neg)
  , (("\\", 1),   bnot)
  , (("^", 2),    pow)
  , (("<<", 2),   lshift)
  , ((">>", 2),   rshift)
  , (("**", 2),   pow)
  , (("div", 2),  div')
  -- , (("rdiv", 2), rdiv) -- requires a data structure for rational numbers
  , (("mod", 2),  mod')
  , (("rem", 2),  rem')
  , (("+", 2),    add)
  , (("-", 2),    sub)
  , (("*", 2),    mul)
  , (("/", 2),    div'')
  ]

plus :: Monad m => Function r m
plus [x] = Prover.assertNumber x >>= return

neg :: Monad m => Function r m
neg [x] = do
  Prover.assertNumber x
  case x of
    PInt v   -> return $ PInt (-v)
    PFloat v -> return $ PFloat (-v)

bnot :: Monad m => Function r m
bnot [x] = do
  PInt v <- Prover.assertPInt x
  return $ PInt (Bits.complement v)

lshift :: Monad m => Function r m
lshift [lhs, rhs] = intBinaryOp f lhs rhs
  where f a b = fromInteger a `Bits.shift` fromInteger b

rshift :: Monad m => Function r m
rshift [lhs, rhs] = intBinaryOp f lhs rhs
  where f a b = fromInteger a `Bits.shiftR` fromInteger b

pow :: Monad m => Function r m
pow [lhs, rhs] = mixedBinaryOp (^) (**) lhs rhs

div' :: Monad m => Function r m
div' [lhs, rhs] = intBinaryOp div lhs rhs

mod' :: Monad m => Function r m
mod' [lhs, rhs] = intBinaryOp mod lhs rhs

rem' :: Monad m => Function r m
rem' [lhs, rhs] = intBinaryOp rem lhs rhs

add :: Monad m => Function r m
add [lhs, rhs] = mixedBinaryOp (+) (+) lhs rhs

sub :: Monad m => Function r m
sub [lhs, rhs] = mixedBinaryOp (-) (-) lhs rhs

mul :: Monad m => Function r m
mul [lhs, rhs] = mixedBinaryOp (*) (*) lhs rhs

div'' :: Monad m => Function r m
div'' [lhs, rhs] = do
  Prover.assertNumber lhs >> Prover.assertNumber rhs
  case (lhs, rhs) of
    (PInt l, PInt r)
      | mod l r == 0 -> return (PInt $ l `div` r)
      | otherwise    -> return (PFloat $ fromInteger l / fromInteger r)
    (PInt l,   PFloat r) -> return (PFloat $ fromInteger l / r)
    (PFloat l, PInt r)   -> return (PFloat $ l / fromInteger r)
    (PFloat l, PFloat r) -> return (PFloat $ l / r)

------------------------------
-- helpful functions
------------------------------

{-# INLINE mixedBinaryOp #-}
mixedBinaryOp :: Monad m => (Integer -> Integer -> Integer) -> (Double -> Double -> Double) ->
  Node -> Node -> ProverT r m Node
mixedBinaryOp fInt fFloat lhs rhs = do
  Prover.assertNumber lhs >> Prover.assertNumber rhs
  case (lhs, rhs) of
    (PInt l,   PInt r)   -> return $ PInt $ fInt l r
    (PInt l,   PFloat r) -> return $ PFloat $ fFloat (fromInteger l) r
    (PFloat l, PInt r)   -> return $ PFloat $ fFloat l               (fromInteger r)
    (PFloat l, PFloat r) -> return $ PFloat $ fFloat l r

{-# INLINE intBinaryOp #-}
intBinaryOp :: Monad m  => (Integer -> Integer -> Integer) -> Node -> Node -> ProverT r m Node
intBinaryOp f lhs rhs = do
  PInt l <- Prover.assertPInt lhs
  PInt r <- Prover.assertPInt rhs
  return (PInt $ f (fromIntegral l) (fromIntegral r))
