module Prolog.Loader (
  loadFile
  ) where

import           Control.Applicative
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State

import           Lib.Parser              (Result(..), runParser, failParse)
import qualified Lib.Parser.Combinator   as Combinator
import           Lib.Parser.String       (StrState(..), beginPos)
import           Lib.Backtrack           (runBacktrackT)
import qualified Lib.Backtrack           as Backtrack

import qualified Prolog.Database  as Database
import qualified Prolog.Parser    as Parser
import qualified Prolog.Prover    as Prover
import qualified Prolog.Node      as Node
import           Prolog.Token     (Token)
import qualified Prolog.Tokenizer as Tokenizer


type PLParserWithEnv m r = Parser.PLParserT (StateT (Prover.Environment r m) m) r


loadFile :: FilePath -> StateT (Prover.Environment () IO) IO ()
loadFile path = do
  content <- lift $ readFile path
  case beginTokenize content of
    Fail msg -> lift $ putStrLn msg
    OK tokens -> do
      opD <- gets Prover.opData
      let beginStream = Parser.TokenStream 0 tokens
      (status, restTokens, opD') <- Parser.runPLParserT loadAll beginStream opD
      Prover.liftOpData $ put opD'
      case status of
        OK () -> lift $ putStrLn ("OK: " ++ path)
        Fail msg -> lift $ do
          putStr $ "loading " ++ path ++ " failed at: "
          putStrLn $ show restTokens
          putStrLn msg
    
loadAll :: Monad m => PLParserWithEnv m ()
loadAll = do
  many loadClause
  Combinator.except Parser.anything (return ()) <|> failParse "parse failed"

------------------------------------------------------------
-- handy functions
------------------------------------------------------------

beginTokenize :: String -> Result [Token]
beginTokenize code =
  let (result, StrState rest pos) = runParser (many Tokenizer.token) (StrState code beginPos) in
    if rest == ""
      then result
      else Fail $ "tokenization failed at " ++ show pos ++ ": " ++ rest

loadClause :: Monad m => PLParserWithEnv m ()
loadClause = do
  clause <- Parser.topLevel
  case clause of
    Node.Func ":-" [node] -> do
      Parser.liftPLParserT $ runBacktrackT (Prover.call node) (return . Backtrack.OK)
      return ()
    _ -> Parser.liftPLParserT . Prover.liftDB $ Database.appendClause clause >> return ()
