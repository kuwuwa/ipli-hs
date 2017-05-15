module Prolog.Loader (
    loadFile
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State

import           Data.Map (Map)
import qualified Data.Map as Map

import           Lib.Combinator   (except)
import           Lib.Parser       (Result(..), runParser, runParserT, failParse)
import           Lib.StringParser (StrState(..), beginPos)

import           Prolog.Database    (Database, Entry, appendClause, parseClause)
import           Prolog.Operator    (OpData(..))
import           Prolog.Parser      (
    TokenStream(..)
  , PLParser
  , PLParserT
  , runPLParserT
  , liftPLParserT
  , topLevel
  , anything
  )
import           Prolog.Prover   (
    Environment(..)
  , liftDB
  , liftPredDB
  , liftOpData
  )
import           Prolog.Token       (Token)
import qualified Prolog.Token       as Tk
import           Prolog.Tokenizer   (token)


loadFile :: FilePath -> StateT (Environment r IO) IO ()
loadFile path = do
  content <- lift $ readFile path
  case beginTokenize content of
    Fail msg -> lift $ putStrLn msg
    OK tokens -> do
      opD <- gets opData
      let beginStream = TokenStream 0 tokens
      (status, restTokens, opD') <- runPLParserT loadAll beginStream opD
      liftOpData $ put opD'
      case status of
        OK () -> lift $ putStrLn ("OK: " ++ path)
        Fail msg -> lift $ do
          putStrLn $ "loading " ++ path ++ " failed at: "
          putStrLn $ show restTokens
    
loadAll :: Monad m => PLParserT (StateT (Environment r m) m) ()
loadAll = do
  many loadClause
  except anything (return ()) <|> failParse "parse failed"

------------------------------------------------------------
-- handy functions
------------------------------------------------------------

beginTokenize :: String -> Result [Token]
beginTokenize code =
  let (result, StrState rest pos) = runParser (many token) (StrState code beginPos)
  in if rest == ""
     then result
     else Fail $ "tokenization failed at " ++ show pos ++ ": " ++ rest

loadClause :: Monad m => PLParserT (StateT (Environment r m) m) ()
loadClause = do
  clause <- topLevel
  -- TODO: execute static procedures (:-)
  liftPLParserT . liftDB $ appendClause clause
  return ()
