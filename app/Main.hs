module Main where

import System.IO
import Lib.Parser
import qualified Prolog

main :: IO ()
main = hSetBuffering stdin LineBuffering >> loop []
  where loop restTokens = do
          putStr ("IPLI " ++ show restTokens ++ ">> ") >> hFlush stdout
          eof <- isEOF
          code <- getLine
          if eof then putStrLn "\nbye"
          else case Prolog.tokenize code of
                 (Fail msg, _) -> do
                   putStrLn $ ">>tokenize failed<< : " ++ msg
                   loop []
                 (OK tokens, restCode) -> do
                   let (asts, restTokens') = Prolog.parse (restTokens ++ tokens)
                   print $ asts
                   loop []
