module Main where

import System.IO
import qualified Prolog

main :: IO ()
main = loop []
  where loop restTokens = do
          putStr ("IPLI " ++ show restTokens ++ ">> ") >> hFlush stdout
          eof <- isEOF
          code <- getLine
          if eof then putStrLn "\nbye" >> return ()
          else case Prolog.tokenize code of
                 (Prolog.Fail msg, _) -> do
                   putStrLn $ ">>tokenize failed<< : " ++ msg
                   loop []
                 (Prolog.OK tokens, restCode) -> do
                   let (asts, restTokens') = Prolog.parse (restTokens ++ tokens)
                   print $ asts
                   loop []
