module Prolog.Operator (
    OpType(..)
  , Operator(..)
  , OpState
  , OpMap
  , OpData(..)
  , addOp
  , mkOpData
  , initOpData
  ) where

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map)
import qualified Data.Map.Strict as Map

import           Control.Monad.Trans.State

------------------------------

data OpType = Fx | Fy | Xf | Yf | Xfx | Xfy | Yfx

instance Show OpType where
  show Fx  = "Fx"
  show Fy  = "Fy"
  show Xf  = "Xf"
  show Yf  = "Yf"
  show Xfx = "Xfx"
  show Xfy = "Xfy"
  show Yfx = "Yfx"

instance Eq OpType where
  Fx  == Fx  = True
  Fy  == Fy  = True
  Xf  == Xf  = True
  Yf  == Yf  = True
  Xfx == Xfx = True
  Xfy == Xfy = True
  Yfx == Yfx = True
  _   == _   = False

data Operator = Operator String Int OpType

instance Show Operator where
  show (Operator name prec opType) =
      "Operator<" ++ name ++ ", " ++ show prec ++ ", " ++ show opType ++ ">"

------------------------------

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
    mkOpData (updZfz $ zfzMap opData) (updFz $ fzMap opData) (updZf $ zfMap  opData)
  where insert = Map.insert opName op
        (updFz, updZf, updZfz)
          | opType `elem` [Fx, Fy] = (insert, id ,id)
          | opType `elem` [Xf, Yf] = (id, insert, id)
          | otherwise              = (id, id, insert)

------------------------------

type OpMap = Map String Operator

initOpData :: OpData
initOpData = mkOpData (Map.fromList binaryOperators)
                      (Map.fromList prefixOperators)
                      (Map.fromList suffixOperators)
  where
    operators = concat . map makeOperators $ [
        (1200, Xfx, ["-->", ":-"])
      , (1200, Fx,  [":-", "?-"])
      , (1100, Xfy, [";", "|"])
      , (1050, Xfy, ["->", "*->"])
      , (1000, Xfy, [","])
      , (990,  Xfx, [":="])
      , (900,  Fy,  ["\\+"])
      , (700,  Xfx, ["<", "=", "=..", "=@=", "\\=@=", "=:=", "=<", "==", "=\\=", ">", ">=",
                     "@<", "@=<", "@>", "@>=", "\\=", "\\==", "as", "is", ">:<", ":<"])
      , (600,  Xfy, [":"])
      , (500,  Yfx, ["+", "-", "/\\", "\\/", "xor"])
      , (500,  Fx,  ["?"])
      , (400,  Yfx, ["*", "/", "//", "div", "rdiv", "<<", ">>", "mod", "rem"])
      , (200,  Xfx, ["**"])
      , (200,  Xfy, ["^"])
      , (200,  Fy,  ["+", "-", "\\"])
      , (100,  Yfx, ["."]) -- used for dicts (not an end of expression)
      , (1,    Fx,  ["$"])
      ]
    binaryOperators = filter (operOneOf [Xfx, Xfy, Yfx]) operators
    prefixOperators = filter (operOneOf [Fx, Fy]) operators
    suffixOperators = filter (operOneOf [Xf, Yf]) operators
    operOneOf opTypes = (`elem` opTypes) . (\(_, Operator _ _ opT) -> opT)
    makeOperators (prec, opType, atoms) = map (\name -> (name, Operator name prec opType)) atoms
