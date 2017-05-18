module Prolog.ProverSpec where

import           Prolog.Node
import           Prolog.Operator (initOpData)
import           Prolog.Prover

import           Lib.Backtrack

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.State (runStateT)

import           Data.Map (Map)
import qualified Data.Map as Map

import           Test.Hspec

import           Debug.Trace

testEnv :: Environment r m
testEnv = Environment {
    bindings     = Map.empty
  , database     = Map.empty
  , predDatabase = Map.empty
  , opData       = initOpData
  , varNum       = 0
}

run :: Monad m => ProverT r m r -> Environment r m -> m (BResult r, Environment r m)
run bt env = runStateT (runBacktrackT bt $ return . OK) env

spec :: Spec
spec = do
  describe "bind" $ do
    it "X = 3" $ do
      (ret, env') <- run (bind (Var "X") (PInt 3)) testEnv
      (ret, bindings env') `shouldBe` (OK (), Map.fromList [("X", PInt 3)])
    it "`X = Y, Y = 42`" $ do
      (ret, env') <- run (bind (Var "X") (Var "Y") >> bind (Var "Y") (PInt 42)) testEnv
      (ret, bindings env') `shouldBe` (OK (), Map.fromList [("X", Var "Y"), ("Y", PInt 42)])

  describe "resolve" $ do
    it "`X = 42` -> resolve \"X\"" $ do
      (ret, _) <- run (resolve (Var "X")) 
                      testEnv { bindings = Map.fromList [("X", PInt 42)] }
      ret `shouldBe` OK (PInt 42)
    it "`X = Y, Y = 42` => resolve (Var \"X\")" $ do
      (ret, env') <- run (resolve (Var "X")) 
                         testEnv { bindings = Map.fromList [("X", Var "Y"), ("Y", PInt 42)] }
      ret `shouldBe` OK (PInt 42)
      bindings env' `shouldBe` Map.fromList [("X", PInt 42), ("Y", PInt 42)]

  describe "unify" $ do
    it "X = 42" $ do
      (ret, env') <- run (unify (Var "X") (PInt 42)) testEnv
      (ret, bindings env') `shouldBe` (OK (), Map.fromList [("X", PInt 42)])
    it "X = Y, Y = 42" $ do
      (ret, env') <- run (unify (Var "X") (Var "Y") >> unify (Var "X") (PInt 42)) testEnv
      (ret, bindings env') `shouldBe` (OK (), Map.fromList [("X", Var "Y"), ("Y", PInt 42)])

  describe "call" $ do
    let okPred _ = return () :: ProverT () IO ()
        failPred = \_ -> failWith "failPred: fail"
        bindPred [lhs, rhs] = bind lhs rhs
    it "get foo/0 in procedure database" $ do
      let env = testEnv { predDatabase = Map.fromList [(("foo", 0), okPred)] }
      (ret, _) <- run (call (Atom "foo")) $ env
      ret `shouldBe` OK ()
    it "foo/0 exit successfully, where `foo :- true.`" $ do
      let env = testEnv {
          predDatabase = Map.fromList [(("true", 0), okPred)]
        , database     = Map.fromList [(("foo", 0), [([], Atom "true")])] }
      (ret, _) <- run (call (Atom "foo")) $ env
      ret `shouldBe` OK ()
    it "foo/0 exit unsuccessfully, where `foo :- fail.`" $ do
      let env = testEnv {
          predDatabase = Map.fromList [(("fail", 0), failPred)]
        , database     = Map.fromList [(("foo", 0), [([], Atom "fail")])] }
      (ret, _) <- run (call (Atom "foo") <|> failWith "failure in test") $ env
      ret `shouldBe` Fail "failure in test"

    it "raise Fatal when there is no predicate in database" $ do
      (ret, _) <- run (call (Atom "foo")) testEnv
      ret `shouldBe` Fatal "no such predicate: foo"

    let xGT3 = do
          PInt x <- resolve $ Var "X" -- XX: rough pattern-matching
          guard (x > 3) <|> failWith "X > 3"

    it "foo(X), where `foo(1). foo(3). foo(2)`, fails under condition X > 3" $ do
      let env = testEnv {
              predDatabase = Map.fromList [(("true", 0), okPred)]
            , database = Map.fromList [
                  (("foo", 1), [ ([PInt 1], Atom "true")
                               , ([PInt 3], Atom "true")
                               , ([PInt 2], Atom "true") ])
              ]
            }
      (ret, _) <- flip run env $ (call (Func "foo" [Var "X"]) >> xGT3) <|> failWith "fail in test"
      ret `shouldBe` Fail "fail in test"

    it "foo(X), where `foo(1). foo(4). foo(2)`, succeeds under condition X > 3" $ do
      let env = testEnv {
              predDatabase = Map.fromList [(("true", 0), okPred)]
            , database = Map.fromList [
                  (("foo", 1), [ ([PInt 1], Atom "true")
                               , ([PInt 4], Atom "true")
                               , ([PInt 2], Atom "true") ])
              ] }
      (ret, _) <- flip run env $ (call (Func "foo" [Var "X"]) >> xGT3) <|> failWith "fail in test"
      ret `shouldBe` OK ()

    it "foo(X), where `foo(B) :- B = 42.`, succeeds with binding X = 10" $ do
      let env = testEnv {
              predDatabase = Map.fromList [ (("=", 2), bindPred) ]
            , database = Map.fromList [ (("foo", 1), [ ([Var "B"], Func "=" [Var "B", PInt 42]) ]) ]
              }
      (ret, env') <- run (call (Func "foo" [Var "X"]) >> resolve (Var "X") >> return ()) env
      -- `resolve (Var "X")` forces indirect reference (s.t. `X -> _G1 -> 42`) into direct one
      (ret, Map.lookup "X" (bindings env')) `shouldBe` (OK (), Just $ PInt 42)
