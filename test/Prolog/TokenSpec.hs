module Prolog.TokenSpec where

import Control.Monad
import Test.Hspec

import Prolog.Token

spec = do
  describe "comparator" $ do
    it "distinguishes atoms and variables" $ do
      Atom "abcdefg" `shouldNotBe` Var "abcdefg"
    it "equates (Atom $ concat [\"a\", \"b\", \"c\"]) and (Atom \"abc\")" $ do
      (Atom $ concat ["a", "b", "c"]) `shouldBe` Atom "abc"
    it "distinguishes (Atom \"ABC\") and (Atom \"abc\")" $ do
      Atom "ABC" `shouldNotBe` Atom "abc"
    it "equates (Var $ concat [\"a\", \"b\", \"c\"]) and (Var \"abc\")" $ do
      (Var $ concat ["a", "b", "c"]) `shouldBe` Var "abc"
    it "distinguishes (Var \"ABC\") and (Var \"abc\")" $ do
      Var "ABC" `shouldNotBe` Var "abc"
    it "distinguishes (PInt 10) and (PFloat 10)" $ do
      PInt 10 `shouldNotBe` PFloat 10
    it "distinguishes left parentheses and right parentheses" $ do
      LParen `shouldNotBe` RParen
    it "equates two `Str`s that have the same content" $ do
      Str ("abc" ++ "def") `shouldBe` Str "abcdef"
