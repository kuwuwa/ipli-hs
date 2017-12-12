module Prolog.Database (
    Database
  , Entry
  , emptyDatabase
  , getPredicates
  , appendClause
  , prependClause
  ) where

import           Control.Monad.Trans.State

import           Data.Map (Map)
import qualified Data.Map.Strict as Map

import           Prolog.Node (Node(..))

------------------------------------------------------------

type Database = Map (Name, Arity) [(Args, Body)]

type Name = String
type Arity = Int

type Args = [Node]
type Body = Node

type Entry = ((Name, Arity), (Args, Body))

------------------------------------------------------------

emptyDatabase :: Database
emptyDatabase = Map.empty

getPredicates :: Monad m => Name -> Arity -> StateT Database m [(Args, Body)]
getPredicates name arity  = do
  resultMaybe <- gets $ Map.lookup (name, arity)
  return $ case resultMaybe of
             Nothing -> []
             Just result -> result

appendClause :: Monad m => Node -> StateT Database m (Either String Entry)
appendClause node = do
  case parseClause node of
    Left msg -> return $ Left msg
    Right (key, val) -> do
      modify' $ append key val
      return $ Right (key, val)
    where append :: Ord k => k -> v -> Map k [v] -> Map k [v]
          append key val mp =
            case Map.lookup key mp of
              Nothing -> Map.insert key [val] mp
              Just _  -> Map.adjust (++ [val]) key mp

prependClause :: Monad m => Node -> StateT Database m (Either String Entry)
prependClause node = do
  case parseClause node of
    Left msg -> return $ Left msg
    Right (key, val) -> do
      modify' (Map.adjust (val:) key)
      return  $ Right (key, val)

------------------------------------------------------------

parseClause :: Node -> Either String Entry
parseClause (Func ":-" [hd, body]) =
  case hd of
    Func name params -> Right ((name, length params), (params, body))
    Atom name        -> Right ((name, 0),             ([], body))
    x -> Left $ "callable expected, but got " ++ show x
parseClause (Func name params) = Right ((name, length params), (params, Atom "true"))
parseClause (Atom name)        = Right ((name, 0), ([], Atom "true"))
parseClause x                  = Left $ "callable expected, but got " ++ show x
