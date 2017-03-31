module Prolog.Procedure (
    ProcDatabase
  , Procedure
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map

import           Prolog.Node (Node)

type Name = String
type Arity = Int

type Args = [Node]

type ProcDatabase = Map (Name, Arity) Procedure

type Procedure = Args -> Temp

data Temp = Temp
