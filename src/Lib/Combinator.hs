module Lib.Combinator where

import Control.Applicative

import Lib.Parser
import Lib.StringParser

option :: Parser s a -> Parser s (Maybe a)
option p = parser $ \st ->
  let (res, st') = runParser p st in
    case res of
      Fail _ -> (OK Nothing, st)
      OK v   -> (OK (Just v), st')

except :: Parser s a -> Parser s b -> Parser s b
except p q = parser $ \st ->
  let (res, _) = runParser p st in
    case res of 
      OK _ -> (Fail "except", st)
      _    -> runParser q st
