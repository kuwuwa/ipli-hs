module Prolog.Operator (
  OpType(..),
  Operator(..),
  Prec,
  OpState,
  OpMap,
  upperPrecLimit,
  lowerPrecLimit,
  OpData(..),
  mkOpData,
  addOp,
  opers,
  readOpType,
  ) where

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Control.Monad.Trans.State

------------------------------

data OpType = Fx | Fy | Xf | Yf | Xfx | Xfy | Yfx

instance Show OpType where
  show Fx  = "fx"
  show Fy  = "fy"
  show Xf  = "xf"
  show Yf  = "yf"
  show Xfx = "xfx"
  show Xfy = "xfy"
  show Yfx = "yfx"

instance Eq OpType where
  Fx  == Fx  = True
  Fy  == Fy  = True
  Xf  == Xf  = True
  Yf  == Yf  = True
  Xfx == Xfx = True
  Xfy == Xfy = True
  Yfx == Yfx = True
  _   == _   = False


data Operator = Operator String Prec OpType

instance Show Operator where
  show (Operator name prec opType) =
      "Operator<" ++ name ++ ", " ++ show prec ++ ", " ++ show opType ++ ">"


type Prec = Int

upperPrecLimit :: Prec
upperPrecLimit = 1200

lowerPrecLimit :: Prec
lowerPrecLimit = 0


type OpState = State OpData

data OpData = OpData {
    zfzMap :: OpMap
  , fzMap :: OpMap
  , zfMap :: OpMap
  , precs :: Set Int
  }

mkOpData :: OpMap -> OpMap -> OpMap -> OpData
mkOpData zfzMap' fzMap' zfMap' = OpData {
    zfzMap = zfzMap'
  , fzMap  = fzMap'
  , zfMap  = zfMap'
  , precs = Set.fromList . concat $ map (map prec . Map.elems) [zfzMap', fzMap', zfMap']
  } where prec (Operator _ p _) = p

addOp :: Operator -> OpData -> OpData
addOp op@(Operator opName _ opType) opData =
  mkOpData (updZfz $ zfzMap opData) (updFz $ fzMap opData) (updZf $ zfMap opData)
  where insert = Map.insert opName op

        (updFz, updZf, updZfz)
          | opType `elem` [Fx, Fy] = (insert, id ,id)
          | opType `elem` [Xf, Yf] = (id, insert, id)
          | otherwise              = (id, id, insert)

opers :: OpData -> [Operator]
opers (OpData { zfzMap = x, fzMap = y, zfMap = z }) = concat $ map Map.elems [x, y, z]

readOpType :: String -> Maybe OpType
readOpType "fx"  = Just Fx
readOpType "fy"  = Just Fy
readOpType "xf"  = Just Xf
readOpType "yf"  = Just Yf
readOpType "xfx" = Just Xfx
readOpType "xfy" = Just Xfy
readOpType "yfx" = Just Yfx
readOpType  _    = Nothing


type OpMap = Map String Operator
