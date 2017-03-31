module Prolog.Builtin.Procedure (
    builtinProcedures
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Prolog.Database
import Prolog.Procedure

builtinProcedures :: ProcDatabase
builtinProcedures = Map.empty
