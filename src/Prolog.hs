module Prolog (
    repl
  ) where

import           Lib.Parser (Result(..), runParser, runParserT, failParse)
import           Lib.Combinator (except)
import           Lib.StringParser (StrState(..), spaces, beginPos)

import           Prolog.Loader      (loadFile)
import           Prolog.AstNode     (AstNode)
import qualified Prolog.AstNode     as AstNode
import           Prolog.Token       (Token)
import qualified Prolog.Token       as Token
import           Prolog.Database    (Database, emptyDatabase)
import           Prolog.Environment (Environment(..))
import           Prolog.Operator    (OpData, initOpData)
import           Prolog.Tokenizer   (tokenize)
import           Prolog.Parser      (TokenStream(..), topLevel)

import           Prolog.Builtin.Procedure (builtinProcedures)

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State

import           Data.Map (Map)
import qualified Data.Map as Map

import           System.Environment
import           System.IO

initEnvironment :: Environment
initEnvironment = Environment { database = Map.empty, procDatabase = builtinProcedures, opData = initOpData }

repl :: IO ()
repl = (fst <$>) $ flip runStateT Prolog.initEnvironment $ do
  args <- lift getArgs
  when (length args > 0) $ do -- load *.pl
    let fileName = args !! 0
    loadFile fileName
  lift (hSetBuffering stdin LineBuffering) >> loop []
  where loop restTokens = do
          db <- gets database
          lift $ print db
          lift $ putStr ("IPLI " ++ show restTokens ++ ">> ") >> hFlush stdout
          eof <- lift isEOF
          code <- lift getLine
          if eof then return ()
          else case tokenize code of
                 (Fail msg, _) -> do
                   lift $ putStrLn $ ">>tokenize failed<< : " ++ msg
                   loop []
                 (OK tokens, restCode) -> do
                   opD <- gets opData
                   let (asts, restTokens') = parse (restTokens ++ tokens) opD
                   lift $ print asts
                   loop []

        parse :: [Token] -> OpData -> ([AstNode], [Token])
        parse tokens opD = 
          let beginStream = TokenStream 0 tokens
              (((OK nodes, TokenStream _ restTokens), opD'), _) =
                runState (runStateT (runParserT (many topLevel) beginStream) opD) Map.empty
          in (nodes, restTokens)
