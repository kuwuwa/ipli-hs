module Prolog.TokenizerSpec where

import Test.Hspec

import Lib.Parser
import Lib.Parser.String

import Prolog.Token
import Prolog.Tokenizer

beginParse p s = runParser p (StrState s beginPos)

spec :: Spec
spec = do
  describe "atom" $ do
    it "parses \"atom\" as (Atom \"atom\")" $ do
      beginParse atom "atom" `shouldBe` (OK $ Atom "atom" False, StrState "" $ Pos 0 4)

    it "parses \"omg!!\" as (Atom \"omg\")" $ do
      beginParse atom "omg!!" `shouldBe`
          (OK $ Atom "omg" False, StrState "!!" $ Pos 0 3)

    let symbolOnly = "+=\\::><..@$^*--#"
    it ("parses " ++ symbolOnly ++ " as (Atom \"" ++ symbolOnly ++ "\")") $ do
      beginParse atom symbolOnly `shouldBe`
          (OK $ Atom symbolOnly False, StrState "" $ Pos 0 (length symbolOnly))

    it "parses \"@#$hoge^&*\" as (Atom \"@#$\")" $ do
      beginParse atom "@#$hoge^&*" `shouldBe`
          (OK $ Atom "@#$" False, StrState "hoge^&*" $ Pos 0 3)

    it "does not parse \"OMG!!\"" $ do
      beginParse atom "OMG!!" `shouldBe`
          (Fail "not an atom", StrState "OMG!!" beginPos)

    it "does not parse \"1a2b\"" $ do
      beginParse atom "1a2b" `shouldBe` (Fail "not an atom", StrState "1a2b" beginPos)

    it "does not parse \"_atom\"" $ do
      beginParse atom "_atom" `shouldBe` (Fail "not an atom", StrState "_atom" beginPos)

    let mess = "*#_-+)(][||2<4l"
    it ("parse '" ++ mess ++ "' as (Atom \"" ++ mess ++ "\")") $ do
      beginParse atom ("'" ++ mess ++ "'") `shouldBe`
          (OK $ Atom mess True, StrState "" $ Pos 0 (length mess + 2))

  describe "var" $ do
    it "parses \"Atom\" as (Var \"Atom\")" $ do
      beginParse var "Atom." `shouldBe` (OK $ Var "Atom", StrState "." (Pos 0 4))

    it "parses \"_123\" as (Var \"_123\")" $ do
      beginParse var "_123" `shouldBe` (OK $ Var "_123", StrState "" (Pos 0 4))

    it "does not parse \"variable\"" $ do
      beginParse var "variable" `shouldBe` (Fail "not a variable", StrState "variable" beginPos)

  describe "num" $ do
    it "parses \"1192\" as (PInt 1192)" $ do
      beginParse num "1192" `shouldBe` (OK $ PInt 1192, StrState "" (Pos 0 4))

    it "parses \"3.141592\" as (PFloat 3.141592)" $ do
      beginParse num "3.141592" `shouldBe` (OK $ PFloat 3.141592, StrState "" $ Pos 0 8)

    it "parses \"6.022e23\" as (PFloat 6.022e23)" $ do
      beginParse num "6.022e23" `shouldBe` (OK $ PFloat 6.022e23, StrState "" $ Pos 0 8)

    it "parses \"-2.71828\" as (PFloat (-2.71828))" $ do
      beginParse num "-2.71828" `shouldBe` (OK $ PFloat (-2.71828), StrState "" $ Pos 0 8)

    it "parses \"-2.\" as (PInt (-2))" $ do
      beginParse num "-2." `shouldBe` (OK $ PInt (-2), StrState "." $ Pos 0 2)

  describe "str" $ do
    it "parses \"I like this book.\" as (Str \"I like this book.\")" $ do
      beginParse str "\"I like this book.\"" `shouldBe`
        (OK $ Str "I like this book.", StrState "" (Pos 0 19))

    let escSeqEx = "\"\\ttab\\n\\aalert\\n\\\"double-quote\\n\\'\\n\""
    it ("parses " ++ escSeqEx) $ do
      beginParse str escSeqEx `shouldBe`
        (OK $ Str "\ttab\n\aalert\n\"double-quote\n'\n", StrState "" (Pos 0 (length escSeqEx)))

    it "parses \"\\\"\\\"\\\"\\\"\\\"\\\"\" as (Str \"\")" $ do
      beginParse str "\"\"\"\"\"\"" `shouldBe`
        (OK $ Str "", StrState "\"\"\"\"" (Pos 0 2))

  describe "lparen" $ do
    it "parses \"(\"" $ do
      beginParse lparen "(" `shouldBe` (OK LParen, StrState "" (Pos 0 1))
    it "does not parse \")\"" $ do
      beginParse lparen ")" `shouldBe` (Fail "not a left parenthesis", StrState ")" beginPos)

  describe "rparen" $ do
    it "does not parse \"(\"" $ do
      beginParse rparen "(" `shouldBe` (Fail "not a right parenthesis", StrState "(" beginPos)
    it "parses \")\"" $ do
      beginParse rparen ")" `shouldBe` (OK RParen, StrState "" (Pos 0 1))

  describe "lbracket" $ do
    it "parses \"[\"" $ do
      beginParse lbracket "[" `shouldBe` (OK LBracket, StrState "" (Pos 0 1))
    it "does not parse \"]\"" $ do
      beginParse lbracket "]" `shouldBe` (Fail "not a left bracket", StrState "]" beginPos)

  describe "rbracket" $ do
    it "does not parse \"[\"" $ do
      beginParse rbracket "[" `shouldBe` (Fail "not a right bracket", StrState "[" beginPos)
    it "parses \"]\"" $ do
      beginParse rbracket "]" `shouldBe` (OK RBracket, StrState "" (Pos 0 1))
