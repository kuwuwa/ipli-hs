module Prolog.Procedure (
    ProcDatabase
  , Procedure
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map

import           Prolog.AstNode (AstNode)

type Name = String
type Arity = Int

type Args = [AstNode]

type ProcDatabase = Map (Name, Arity) Procedure

type Procedure = Args -> Temp

data Temp = Temp
