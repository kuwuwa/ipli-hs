module Prolog.ProverSpec where

import           Prolog.Node             (Node(Atom, PInt, PFloat, Var, Str, Func))
import           Prolog.Prover
import           Prolog.Builtin.Operator (builtinOpData)

import           Lib.Backtrack (BacktrackT(..), Result(OK, Fail, Fatal, Cut), failWith)

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State (runStateT, evalStateT, gets)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Test.Hspec

testEnv :: Environment r m
testEnv = Environment {
  bindings     = Map.empty,
  database     = Map.empty,
  predDatabase = Map.empty,
  funcDatabase = Map.empty,
  opData       = builtinOpData,
  varNum       = 0
}

run :: Monad m => ProverT r m r -> Environment r m -> m (Result r)
run bt env = evalStateT (runBacktrackT bt $ return . OK) env

getBindings :: Monad m => ProverT r m Bindings
getBindings = lift $ gets bindings

attachBindings :: Monad m => a -> ProverT (a, Bindings) m (a, Bindings)
attachBindings v = do
  bd <- getBindings
  return (v, bd)

spec :: Spec
spec = do
  describe "bind" $ do
    it "X = 3" $ do
      bd <- run (bind (Var "X") (PInt 3) >> getBindings) testEnv
      bd `shouldBe` OK (Map.fromList [("X", PInt 3)])
    it "`X = Y, Y = 42`" $ do
      bd <- run (bind (Var "X") (Var "Y") >> bind (Var "Y") (PInt 42) >> getBindings) testEnv
      bd `shouldBe` OK (Map.fromList [("X", Var "Y"), ("Y", PInt 42)])

  describe "resolve" $ do
    it "`X = 42` -> resolve \"X\"" $ do
      ret <- run (resolve (Var "X")) 
                 testEnv { bindings = Map.fromList [("X", PInt 42)] }
      ret `shouldBe` OK (PInt 42)
    it "`X = Y, Y = 42` => X = 42" $ do
      OK x <- run (resolve (Var "X")) 
                          testEnv { bindings = Map.fromList [("X", Var "Y"), ("Y", PInt 42)] }
      x `shouldBe` PInt 42

  describe "unify" $ do
    it "X = 42" $ do
      bd <- run (unify (Var "X") (PInt 42) >> getBindings) testEnv
      bd `shouldBe` OK (Map.fromList [("X", PInt 42)])
    it "X = Y, Y = 42" $ do
      bd <- run (unify (Var "X") (Var "Y") >> unify (Var "X") (PInt 42) >> getBindings) testEnv
      bd `shouldBe` OK (Map.fromList [("X", Var "Y"), ("Y", PInt 42)])
    it "`foo(1, bar(Z, 4)) = foo(X, bar(Y, Y))` => X = 1, Y = Z = 4`" $ do
      let u = unify (Func "foo" [PInt 1, Func "bar" [Var "Z", PInt 4]])
                    (Func "foo" [Var "X", Func "bar" [Var "Y", Var "Y"]])
              >> mapM resolve [Var "X", Var "Y", Var "Z"]
      res <- run u testEnv
      res `shouldBe` OK [PInt 1, PInt 4, PInt 4]

  describe "call" $ do
    let okPred _ = return () :: ProverT () IO ()
        failPred = \_ -> failWith "failPred: fail"
        unifyP [lhs, rhs] = bind lhs rhs
    it "get foo/0 in procedure database" $ do
      let env = testEnv { predDatabase = Map.fromList [(("foo", 0), okPred)] }
      ret <- run (call (Atom "foo")) $ env
      ret `shouldBe` OK ()
    it "foo/0 exit successfully, where `foo :- true.`" $ do
      let env = testEnv {
          predDatabase = Map.fromList [(("true", 0), okPred)]
        , database     = Map.fromList [(("foo", 0), [([], Atom "true")])] }
      ret <- run (call (Atom "foo")) $ env
      ret `shouldBe` OK ()
    it "foo/0 exit unsuccessfully, where `foo :- fail.`" $ do
      let env = testEnv {
          predDatabase = Map.fromList [(("fail", 0), failPred)]
        , database     = Map.fromList [(("foo", 0), [([], Atom "fail")])] }
      ret <- run (call (Atom "foo") <|> failWith "failure in test") $ env
      ret `shouldBe` Fail "failure in test"

    it "raise Fatal when there is no predicate in database" $ do
      run (call (Atom "foo")) testEnv >>= (`shouldBe` Fatal "Undefined procedure: foo/0")

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
      ret <- flip run env $ (call (Func "foo" [Var "X"]) >> xGT3) <|> failWith "!!fail in test!!"
      ret `shouldBe` Fail "!!fail in test!!"

    it "foo(X), where `foo(1). foo(4). foo(2)`, succeeds under condition X > 3" $ do
      let env = testEnv {
              predDatabase = Map.fromList [(("true", 0), okPred)]
            , database = Map.fromList [
                  (("foo", 1), [ ([PInt 1], Atom "true")
                               , ([PInt 4], Atom "true")
                               , ([PInt 2], Atom "true") ])
              ] }
      ret <- flip run env $ (call (Func "foo" [Var "X"]) >> xGT3) <|> failWith "fail in test"
      ret `shouldBe` OK ()

    it "foo(X), where `foo(B) :- B = 42.`, succeeds with binding X = 42" $ do
      let env = testEnv {
              predDatabase = Map.fromList [ (("=", 2), unifyP) ]
            , database = Map.fromList [ (("foo", 1), [ ([Var "B"], Func "=" [Var "B", PInt 42]) ]) ]
            }
      OK x <- run (call (Func "foo" [Var "X"]) >> resolve (Var "X")) env
      x `shouldBe` (PInt 42)
