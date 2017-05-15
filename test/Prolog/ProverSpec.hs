module Prolog.ProverSpec where

import           Prolog.Node
import           Prolog.Operator (initOpData)
import           Prolog.Prover

import           Lib.Backtrack

import           Control.Monad.Trans.State (runStateT)

import           Data.Map (Map)
import qualified Data.Map as Map

import           Test.Hspec

emptyEnv :: Environment r m
emptyEnv = Environment {
    bindings     = Map.empty
  , database     = Map.empty
  , predDatabase = Map.empty
  , opData       = initOpData
}

run :: Monad m => ProverT r m r -> Environment r m -> m (BResult r, Environment r m)
run bt env = runStateT (runBacktrackT bt $ return . OK) env

spec :: Spec
spec = do
  describe "bind" $ do
    it "X = 3" $ do
      (ret, env') <- run (bind (Var "X") (PInt 3)) emptyEnv
      (ret, bindings env') `shouldBe` (OK (), Map.fromList [("X", PInt 3)])
    it "`X = Y, Y = 42`" $ do
      (ret, env') <- run (bind (Var "X") (Var "Y") >> bind (Var "Y") (PInt 42)) emptyEnv
      (ret, bindings env') `shouldBe` (OK (), Map.fromList [("X", Var "Y"), ("Y", PInt 42)])

  describe "resolve" $ do
    it "`X = 42` -> resolve \"X\"" $ do
      (ret, _) <- run (resolve (Var "X")) 
                      emptyEnv { bindings = Map.fromList [("X", PInt 42)] }
      ret `shouldBe` OK (PInt 42)
    it "`X = Y, Y = 42` => resolve (Var \"X\")" $ do
      (ret, env') <- run (resolve (Var "X")) 
                         emptyEnv { bindings = Map.fromList [("X", Var "Y"), ("Y", PInt 42)] }
      ret `shouldBe` OK (PInt 42)
      bindings env' `shouldBe` Map.fromList [("X", PInt 42), ("Y", PInt 42)]

  describe "unify" $ do
    it "X = 42" $ do
      (ret, env') <- run (unify (Var "X") (PInt 42)) emptyEnv
      (ret, bindings env') `shouldBe` (OK (), Map.fromList [("X", PInt 42)])
    it "X = Y, Y = 42" $ do
      (ret, env') <- run (unify (Var "X") (Var "Y") >> unify (Var "X") (PInt 42)) emptyEnv
      (ret, bindings env') `shouldBe` (OK (), Map.fromList [("X", Var "Y"), ("Y", PInt 42)])
