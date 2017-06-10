module Prolog (
    repl
  ) where

import           Lib.Parser       (Result(..), runParser, runParserT, failParse)
import           Lib.Combinator   (except)
import           Lib.StringParser (StrState(..), spaces, beginPos)
import           Lib.Backtrack    (BacktrackT(..), failWith)
import qualified Lib.Backtrack    as Backtrack

import           Prolog.Loader    (loadFile)
import           Prolog.Node      (Node(..))
import qualified Prolog.Node      as Node
import           Prolog.Token     (Token)
import qualified Prolog.Token     as Token
import           Prolog.Database  (Database, emptyDatabase, appendClause)
import           Prolog.Operator  (OpData, initOpData)
import           Prolog.Parser    (TokenStream(..), runPLParser, topLevel)
import           Prolog.Prover    (Environment(..), liftDB, call)
import           Prolog.Tokenizer (tokenize)

import           Prolog.Builtin.Predicate (builtinPredicates)

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State

import           Data.Char
import           Data.Map (Map)
import qualified Data.Map as Map

import           System.Environment
import           System.IO

initEnvironment :: Monad m => Environment r m
initEnvironment = Environment {
    bindings = Map.empty
  , database = Map.empty
  , predDatabase = builtinPredicates
  , varNum = 0
  , opData = initOpData
  }

repl :: IO ()
repl = (fst <$>) $ flip runStateT initEnvironment $ do
  args <- lift getArgs
  when (length args > 0) $ do -- load *.pl
    let fileName = args !! 0
    loadFile fileName
  lift (hSetBuffering stdin LineBuffering) >> loop []
  where loop restTokens = do
          lift $ putStr "IPLI > " >> hFlush stdout
          eof <- lift isEOF
          if eof then lift $ putStrLn "bye"
          else do
            code <- lift getLine
            case fst $ tokenize code of
              Fail msg -> do
                lift $ putStrLn $ ">>tokenize failed<< : " ++ msg
                loop []
              OK tokens -> do
                (asts, restTokens') <- parse (restTokens ++ tokens) <$> gets opData
                runBacktrackT (sequence_ $ map execClause asts) (return . Backtrack.OK)

                let dropUnparsed rest = do
                      let rest' = dropWhile (/= Token.Period) rest
                      if rest' == [] then return rest
                      else do
                        let stuff = takeWhile (/= Token.Period) rest
                        putStrLn $ "couldn't parse: " ++ show stuff
                        dropUnparsed rest'

                tokens' <- lift $ dropUnparsed restTokens'
                loop tokens'

        execClause clause = lift $ do
          case clause of
            Func ":-" [node] -> do
              status <- runBacktrackT (call node >> ask) (return . Backtrack.OK)
              lift . putStrLn $ case status of
                Backtrack.OK () -> "" -- nope
                Backtrack.Fail msg -> "[IPLI] failed: " ++ msg
                Backtrack.Fatal msg -> "[IPLI] error: " ++ msg
            _ -> do
              liftDB $ appendClause clause
              lift $ putStrLn "[IPLI] registered"

        ask = do
          bs <- lift $ Map.assocs <$> gets bindings
          mapM_ printBinding bs
          ok <- lift . lift $ do
            putStr "[y/N]: " >> hFlush stdout
            yn <- getLine
            return $ map toLower yn `elem` ["y", "yes"]
          if ok
          then return ()
          else failWith "this is not what you want"
            where printBinding (key, val) = lift . lift . putStrLn $ key ++ " = " ++ show val

        parse :: [Token] -> OpData -> ([Node], [Token])
        parse tokens opD = 
          let beginStream = TokenStream 0 tokens
              (OK nodes, TokenStream _ restTokens, opD') = runPLParser (many topLevel) beginStream opD
          in (nodes, restTokens)
