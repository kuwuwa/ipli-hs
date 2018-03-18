module Prolog.Repl (
    repl
  ) where

import           Lib.Parser       (Result(..))
import           Lib.Backtrack    (BacktrackT(..), failWith)
import qualified Lib.Backtrack    as Backtrack

import           Prolog.Loader    (loadFile)
import           Prolog.Node      (Node(..))
import           Prolog.Token     (Token)
import qualified Prolog.Token     as Token
import           Prolog.Database  (appendClause)
import           Prolog.Operator  (OpData, initOpData)
import           Prolog.Parser    (TokenStream(..), runPLParser, topLevel)
import           Prolog.Prover    (Environment(..), ProverT, liftDB, call, unparse)
import           Prolog.Tokenizer (tokenize)

import           Prolog.Builtin.Predicate (builtinPredicates, ioPredicates)
import           Prolog.Builtin.Function (builtinFuncs)

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State

import           Data.Char
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set

import           System.Environment
import           System.IO

initEnvironment :: MonadIO m => Environment r m
initEnvironment = Environment {
    bindings = Map.empty
  , database = Map.empty
  , predDatabase = Map.union builtinPredicates ioPredicates
  , funcDatabase = builtinFuncs
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
  where
    loop :: [Token] -> StateT (Environment () IO) IO ()
    loop restTokens = do
      lift $ putStr "IPLI > " >> hFlush stdout
      eof <- lift isEOF
      if eof then lift $ putStrLn "bye"
      else do
        code <- lift getLine
        case fst $ tokenize code of
          Fail msg -> do
            lift $ putStrLn $ ">>tokenize failed<< : " ++ msg
            loop []
          OK tokens -> execute (restTokens ++ tokens) >>= loop

    execute :: [Token] -> StateT (Environment () IO) IO [Token]
    execute tokens = do
      (res, tokens') <- parse1 tokens <$> gets opData
      case res of
        Fail _ -> do
          let rest = dropWhile (/= Token.Period) tokens
          if rest == [] then return tokens
          else do
            let stuff = takeWhile (/= Token.Period) tokens
            lift $ putStrLn $ "can't parse: " ++ show stuff
            execute $ tail rest
        OK ast -> do
          runBacktrackT (execClause ast) (return . Backtrack.OK)
          execute tokens'

    parse1 :: [Token] -> OpData -> (Result Node, [Token])
    parse1 tokens opD =
      let beginStream = TokenStream 0 tokens
          (res, TokenStream _ restTokens, _) = runPLParser topLevel beginStream opD
      in (res, restTokens)

    execClause :: Node -> ProverT () IO ()
    execClause clause = lift $ do
      case clause of
        _ -> do
          let shownVars = collectVars clause
          status <- runBacktrackT (call clause >> ask shownVars) (return . Backtrack.OK)
          lift . putStrLn $ case status of
            Backtrack.OK () -> "[IPLI] true"
            Backtrack.Fail msg -> "[IPLI] false"
            Backtrack.Fatal msg -> "[IPLI] error: " ++ msg
        -- TODO: operator registration

    ask :: Set String -> ProverT () IO ()
    ask shown = do
      bs <- lift $ filter (flip Set.member shown . fst) <$> Map.assocs <$> gets bindings
      mapM_ printBinding bs
      yes <- lift . lift $ do
        putStr "[y/N]: " >> hFlush stdout
        yn <- getLine
        return $ map toLower yn `elem` ["y", "yes"]
      if yes
      then return ()
      else failWith "there is not what you want"
        where
          printBinding (key, val) = do
            lift (unparse (Func "=" [Var key, val])) >>= lift . lift . putStrLn

    collectVars (Var v)       = Set.singleton v
    collectVars (Func _ args) = foldr Set.union Set.empty $ map collectVars args
    collectVars _             = Set.empty
