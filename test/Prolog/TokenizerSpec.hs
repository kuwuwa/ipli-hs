module Prolog.TokenizerSpec where

import Test.Hspec

import Lib.Parser
import Prolog.Token
import Prolog.Tokenizer

beginParse p s = runParser p (PState s beginPos)

spec :: Spec
spec = do
  describe "atom" $ do
    it "parse \"atom\" as (Atom \"atom\")" $ do
      beginParse atom "atom" `shouldBe` (OK $ Atom "atom", PState "" $ Pos 0 4)

    it "parse \"omg!!\" as (Atom \"omg\")" $ do
      beginParse atom "omg!!" `shouldBe`
          (OK $ Atom "omg", PState "!!" $ Pos 0 3)

    let symbolOnly = "+=\\::><..@$^*--#"
    it ("parse " ++ symbolOnly ++ " as (Atom \"" ++ symbolOnly ++ "\")") $ do
      beginParse atom symbolOnly`shouldBe`
          (OK $ Atom symbolOnly, PState "" $ Pos 0 (length symbolOnly))

    it "parse \"@#$hoge^&*\" as (Atom \"@#$\")" $ do
      beginParse atom "@#$hoge^&*" `shouldBe`
          (OK $ Atom "@#$", PState "hoge^&*" $ Pos 0 3)

    it "does not parse \"OMG!!\"" $ do
      beginParse atom "OMG!!" `shouldBe`
          (Fail "not an atom", PState "OMG!!" beginPos)

    it "does not parse \"1a2b\"" $ do
      beginParse atom "1a2b" `shouldBe` (Fail "not an atom", PState "1a2b" beginPos)

    it "does not parse \"_atom\"" $ do
      beginParse atom "_atom" `shouldBe` (Fail "not an atom", PState "_atom" beginPos)

    let mess = "*#_-+)(][||2<4l"
    it ("parse '" ++ mess ++ "' as (Atom \"" ++ mess ++ "\")") $ do
      beginParse atom ("'" ++ mess ++ "'") `shouldBe`
          (OK $ Atom mess, PState "" $ Pos 0 (length mess + 2))

  describe "var" $ do
    it "parse \"Atom\" as (Var \"Atom\")" $ do
      beginParse var "Atom." `shouldBe` (OK $ Var "Atom", PState "." (Pos 0 4))

    it "parse \"_123\" as (Var \"_123\")" $ do
      beginParse var "_123" `shouldBe` (OK $ Var "_123", PState "" (Pos 0 4))

    it "does not parse \"variable\"" $ do
      beginParse var "variable" `shouldBe` (Fail "not a variable", PState "variable" beginPos)

  describe "num" $ do
    it "parse \"1192\" as (PInt 1192)" $ do
      beginParse num "1192" `shouldBe` (OK $ PInt 1192, PState "" (Pos 0 4))

    it "parse \"3.141592\" as (PFloat 3.141592)" $ do
      beginParse num "3.141592" `shouldBe` (OK $ PFloat 3.141592, PState "" $ Pos 0 8)

    it "parse \"6.022e23\" as (PFloat 6.022e23)" $ do
      beginParse num "6.022e23" `shouldBe` (OK $ PFloat 6.022e23, PState "" $ Pos 0 8)

    it "parse \"-2.71828\" as (PFloat (-2.71828))" $ do
      beginParse num "-2.71828" `shouldBe` (OK $ PFloat (-2.71828), PState "" $ Pos 0 8)

    it "parse \"-2.\" as (PInt (-2))" $ do
      beginParse num "-2." `shouldBe` (OK $ PInt (-2), PState "." $ Pos 0 2)

  describe "str" $ do
    it "parse \"I like this book.\" as (Str \"I like this book.\")" $ do
      beginParse str "\"I like this book.\"" `shouldBe`
        (OK $ Str "I like this book.", PState "" (Pos 0 19))

    let escSeqEx = "\"\\ttab\\n\\aalert\\n\\\"double-quote\\n\\'\\n\""
    it ("parse " ++ escSeqEx) $ do
      beginParse str escSeqEx `shouldBe`
        (OK $ Str "\ttab\n\aalert\n\"double-quote\n'\n", PState "" (Pos 0 (length escSeqEx)))

    it "parse \"\\\"\\\"\\\"\\\"\\\"\\\"\" as (Str \"\")" $ do
      beginParse str "\"\"\"\"\"\"" `shouldBe`
        (OK $ Str "", PState "\"\"\"\"" (Pos 0 2))

  describe "lparen" $ do
    it "parse \"(\"" $ do
      beginParse lparen "(" `shouldBe` (OK LParen, PState "" (Pos 0 1))
    it "does not parse \")\"" $ do
      beginParse lparen ")" `shouldBe` (Fail "not a left parenthesis", PState ")" beginPos)

  describe "rparen" $ do
    it "does not parse \"(\"" $ do
      beginParse rparen "(" `shouldBe` (Fail "not a right parenthesis", PState "(" beginPos)
    it "parse \")\"" $ do
      beginParse rparen ")" `shouldBe` (OK RParen, PState "" (Pos 0 1))

  describe "lbracket" $ do
    it "parse \"[\"" $ do
      beginParse lbracket "[" `shouldBe` (OK LBracket, PState "" (Pos 0 1))
    it "does not parse \"]\"" $ do
      beginParse lbracket "]" `shouldBe` (Fail "not a left bracket", PState "]" beginPos)

  describe "rbracket" $ do
    it "does not parse \"[\"" $ do
      beginParse rbracket "[" `shouldBe` (Fail "not a right bracket", PState "[" beginPos)
    it "parse \"]\"" $ do
      beginParse rbracket "]" `shouldBe` (OK RBracket, PState "" (Pos 0 1))
