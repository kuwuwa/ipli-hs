module Prolog.Unparser (
    unparse
) where

import           Prolog.Node   (Node(..))
import           Prolog.Parser (upperPrecLimit)
import           Prolog.Prover  
import           Prolog.Operator

import           Control.Monad.Trans.State

import qualified Data.Map as Map

unparse :: Monad m => Node -> StateT (Environment r m) m String
unparse (Atom a)   = return $ a -- TODO: quote when necessary
unparse (Var v)    = return $ v
unparse (PInt i)   = return $ show i
unparse (PFloat f) = return $ show f
unparse (Str s)    = return $ s
unparse Nil        = return "[]"
unparse _func@(Func "[|]" [_, _]) = unparseList _func
unparse _func@(Func _ _) = unparseFunc upperPrecLimit _func
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
      if isListLit func then unparseList func
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

    unparseFunc prec func@(Func _ _) = unparseFuncDefault func

    unparseFunc _ term = unparse term

    unparseFuncDefault (Func name args) = do
      -- 1000 is the precedence of comma
      argsStr <- mapM (unparseFunc $ pred 1000) args
      return $ name ++ "(" ++ join ", " argsStr ++ ")"

    isListLit (Func "[|]" [_,_]) = True
    isListLit _ = False

    join _ [] = ""
    join delim (x:xs) = concat $ x : zipWith (++) (repeat delim) xs

unparseList (Func "[|]" [hd, tl]) = do
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
