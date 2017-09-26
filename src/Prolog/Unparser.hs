module Prolog.Unparser (
    unparse
) where

import           Prolog.Node   (Node(..))
import           Prolog.Prover  
import           Prolog.Operator

import           Control.Monad.Trans.State

import qualified Data.Map as Map

unparse :: Monad m => Node -> StateT (Environment r m) m String
unparse (Atom a)   = return $ show a
unparse (Var v)    = return $ show v
unparse (PInt i)   = return $ show i
unparse (PFloat f) = return $ show f
unparse (Str s)    = return $ show s
unparse Nil        = return "[]"
unparse (Func "[|]" [hd, tl]) = do
  hdStr <- unparse hd 
  ("[" ++) <$> (hdStr ++) <$> unparseTail tl
    where
      unparseTail Nil = return "]"
      unparseTail (Func "[|]" [hd, tl]) = do
          hdStr <- unparse hd
          (", " ++) <$> (hdStr ++) <$> unparseTail tl
      unparseTail term = do
          termStr <- unparse term
          return $ " | " ++ termStr ++ "]"
unparse func@(Func _ _) = unparseFunc 700 func
  where
    unparseFunc prec fc@(Func name [term]) = do
      fzOpMaybe <- liftOpData $ gets (Map.lookup name . fzMap)
      zfOpMaybe <- liftOpData $ gets (Map.lookup name . zfMap)
      case (fzOpMaybe, zfOpMaybe) of
        (Just (Operator _ prec' opType), _)
          | opType == Fx -> error "TODO"
          | opType == Fy -> error "TODO"
        (_, Just (Operator _ prec' opType))
          | opType == Xf -> error "TODO"
          | opType == Yf -> error "TODO"
        _ -> unparseFuncDefault fc
    unparseFunc prec fc@(Func name [lhs, rhs]) = do
      zfzOpMaybe <- liftOpData $ gets (Map.lookup name . zfzMap)
      case zfzOpMaybe of
        Just (Operator _ prec' opType)
          | opType == Xfx -> error "TODO"
          | opType == Xfy -> error "TODO"
          | opType == Yfx -> error "TODO"
        _ -> unparseFuncDefault fc
    unparseFunc _ fc = unparseFuncDefault fc

    unparseFuncDefault (Func name args) = do
      argsStr <- mapM unparse args
      return $ name ++ "(" ++ join ", " argsStr ++ ")"

    join _ [] = ""
    join delim (x:xs) = concat  $ x : zipWith (++) (repeat delim) xs
