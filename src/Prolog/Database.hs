module Prolog.Database (
  Database,
  Entry,
  emptyDatabase,
  getPredicates,
  appendClause,
  prependClause,
  ) where

import           Control.Monad.Trans.State

import           Data.Map.Strict (Map)
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
getPredicates name arity = gets $ Map.findWithDefault [] (name, arity)

appendClause :: Monad m => Node -> StateT Database m (Either String Entry)
appendClause node = do
  case parseClause node of
    Left msg -> return $ Left msg
    Right (key, val) -> do
      modify' $ append key val
      return $ Right (key, val)
    where append :: Ord k => k -> v -> Map k [v] -> Map k [v]
          append key val mp =
            let xs = Map.findWithDefault [] key mp in
            Map.insert key (xs ++ [val]) mp

prependClause :: Monad m => Node -> StateT Database m (Either String Entry)
prependClause node = do
  case parseClause node of
    Left msg -> return $ Left msg
    Right (key, val) -> do
      modify' $ prepend key val
      return $ Right (key, val)
    where prepend :: Ord k => k -> v -> Map k [v] -> Map k [v]
          prepend key val mp =
            let xs = Map.findWithDefault [] key mp in
            Map.insert key (val:xs) mp

------------------------------------------------------------

parseClause :: Node -> Either String Entry
parseClause node =
  case node of
    Func ":-" [hd, body] -> aux hd body
    _ -> aux node (Atom "true")
  where
    aux hd body =
      case hd of
        Func name params -> Right ((name, length params), (params, body))
        Atom name -> Right ((name, 0), ([], body))
        x -> Left $ "callable expected, but got " ++ show x
