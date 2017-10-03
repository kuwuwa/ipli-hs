module Prolog.ParserSpec where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.State

import           Data.Map (Map)
import qualified Data.Map as Map

import           Test.Hspec

import           Lib.Combinator
import           Lib.Parser
import           Lib.StringParser

import           Prolog.Node  (Node(..))
import           Prolog.Operator 
import           Prolog.Token    (Token)
import qualified Prolog.Token    as Tk

import           Prolog.Parser (PLParser, TokenStream(..), upperPrecLimit)
import qualified Prolog.Parser as Parser

parse :: PLParser a -> [Token] -> (Result a, TokenStream)
parse parser tokens =
  let initTkStream = TokenStream 0 tokens
      (((result, stm'), _), _) =
        runState (runStateT (runParserT parser initTkStream) initOpData) Map.empty
  in (result, stm')

spec :: Spec
spec = do
  let pair a b = Func "[|]" [a, b]
  describe "topLevel" $ do
    let parseTopLevel = parse Parser.topLevel
    it "\"1 / 2.\" -> [(Func / (PInt 1) (PInt 2))]" $ do
      let tokens = [Tk.PInt 1, Tk.Atom "/" False, Tk.PInt 2, Tk.Period]
      parseTopLevel tokens `shouldBe`
          (OK $ Func "/" [(PInt 1), (PInt 2)], TokenStream 4 [])

    it "\"1 + 2 - 3.\" -> [(Func - (Func + (PInt 1) (PInt 2)) (PInt 3))]" $ do
      let tokens = [Tk.PInt 1, Tk.Atom "+" False, Tk.PInt 2, Tk.Atom "-" False, Tk.PInt 3, Tk.Period]
      parseTopLevel tokens `shouldBe`
          (OK $ Func "-" [(Func "+" [(PInt 1), (PInt 2)]), (PInt 3)], TokenStream 6 [])

    it "\"10 + 78 div 3.\" -> [(Func + (PInt 10) (Func div (PInt 78) (PInt 3)))]" $ do
      let tokens = [Tk.PInt 10, Tk.Atom "+" False, Tk.PInt 78, Tk.Atom "div" False, Tk.PInt 3, Tk.Period]
      parseTopLevel tokens `shouldBe`
          (OK $ Func "+" [PInt 10,  (Func "div" [(PInt 78), (PInt 3)])], TokenStream 6 [])

    it "\"4.1 * (5.9 + 80.8).\" -> [(Func * (PInt 4) (Func + (PInt 5) (PInt 80)))]" $ do
      let tokens = [Tk.PFloat 4.1, Tk.Atom "*" False, Tk.LParen, Tk.PFloat 5.9,
                    Tk.Atom "+" False, Tk.PFloat 80.8, Tk.RParen, Tk.Period]
      parseTopLevel tokens `shouldBe`
          (OK $ Func "*" [ PFloat 4.1, Func "+" [ PFloat 5.9, PFloat 80.8 ] ], TokenStream 8 [])

    it "\"- - - 42.\" -> [ (Func - (Func - (Func - (PInt 2)))) ]" $ do
      let tokens = [Tk.Atom "-" False, Tk.Atom "-" False, Tk.Atom "-" False, Tk.PInt 42, Tk.Period]
      parseTopLevel tokens `shouldBe`
          (OK $ Func "-" [Func "-" [Func "-" [PInt 42]]], TokenStream 5 [])

    it "\"+ 1 + 2.\" -> [ (Func + (Func + (PInt 1)) (PInt 2)) ]" $ do
      let tokens = [Tk.Atom "+" False, Tk.PInt 1, Tk.Atom "+" False, Tk.PInt 2, Tk.Period]
      parseTopLevel tokens `shouldBe`
          (OK $ Func "+" [Func "+" [PInt 1], PInt 2], TokenStream 5 [])

    it "\"7 'mod' 2.\" -> [] (∵ quoted atoms cannot be operators)" $ do
      let tokens = [Tk.PInt 7, Tk.Atom "mod" True, Tk.PInt 2, Tk.Period]
      parse (Parser.topLevel <|> failParse "parse failed") tokens `shouldBe`
          (Fail "parse failed", TokenStream 0 [Tk.PInt 7, Tk.Atom "mod" True, Tk.PInt 2, Tk.Period])

    it "meow(X) :- cat(X), alive(X)." $ do
      let tokens = [Tk.Func "meow" , Tk.Var "X", Tk.RParen,
                    Tk.Atom ":-" False, Tk.Func "cat" , Tk.Var "X", Tk.RParen,
                    Tk.Atom "," False, Tk.Func "alive", Tk.Var "X", Tk.RParen, Tk.Period]
      parseTopLevel tokens `shouldBe`
          (OK $ Func ":-" [ Func "meow" [Var "X"],
                            Func "," [ Func "cat" [Var "X"], Func "alive" [Var "X"] ] ],
           TokenStream (length tokens) [])

    it "[1, 2, 3, 4]." $ do
      let tokens = [Tk.LBracket, Tk.PInt 1, Tk.Atom "," False, Tk.PInt 2, Tk.Atom "," False,
                    Tk.PInt 3, Tk.Atom "," False, Tk.PInt 4, Tk.RBracket, Tk.Period]
      parseTopLevel tokens `shouldBe`
          (OK $ pair (PInt 1) (pair (PInt 2) (pair (PInt 3) (pair (PInt 4) Nil) ) ),
          TokenStream (length tokens) [])

    it "[\"foo\", \"bar\" | [7, 5]]." $ do
      let tokens = [Tk.LBracket, Tk.Str "foo", Tk.Atom "," False,Tk.Str "bar", Tk.Bar,
                    Tk.LBracket, Tk.PInt 7, Tk.Atom "," False, Tk.PInt 5, Tk.RBracket, Tk.RBracket, Tk.Period]
      parseTopLevel tokens `shouldBe`
          (OK $ pair (Str "foo") (pair (Str "bar") (pair (PInt 7) (pair (PInt 5) Nil) ) ),
           TokenStream (length tokens) [])

    it "[car | cdr]." $ do
      let tokens = [Tk.LBracket, Tk.Atom "car" False, Tk.Bar, Tk.Atom "cdr" False, Tk.RBracket, Tk.Period]
      parseTopLevel tokens `shouldBe`
          (OK $ pair (Atom "car") (Atom "cdr"), TokenStream 6 [])

    it "foo(bar, baz(qux))." $ do
      let tokens = [Tk.Func "foo", Tk.Atom "bar" False, Tk.Atom "," False,
                    Tk.Func "baz", Tk.Atom "qux" False, Tk.RParen, Tk.RParen, Tk.Period]
      parseTopLevel tokens `shouldBe`
          (OK $ Func "foo" [ Atom "bar", Func "baz" [ Atom "qux" ] ],
           TokenStream (length tokens) [])
