module Lib.Combinator where

import Control.Applicative

import Lib.Parser

oneOf :: [Char] -> Parser Char
oneOf = foldl (<|>) failParser . map exact

option :: Parser a -> Parser (Maybe a)
option p = parser $ \st ->
  let (res, st') = runParser p st in
    case res of
      Fail _ -> (OK Nothing, st)
      OK v   -> (OK (Just v), st')

consumeBy :: Parser a -> Parser String
consumeBy p = parser $ \st ->
  let (res, st') = runParser p st in
    case res of
      Fail msg -> (Fail msg, st')
      OK v     -> (OK $ collect st st', st')
  where collect st@(PState (x:xs) posX) dest@(PState _ posY)
          | posX == posY = []
          | otherwise = let (_, st') = runParser char st
                        in x : collect st' dest
          
except :: Parser a -> Parser b -> Parser b
except p q = parser $ \st ->
  let (res, _) = runParser p st in
    case res of 
      OK _ -> (Fail "except", st)
      _    -> runParser q st
