module Prolog.Builtin.Operator (builtinOpData) where

import           Prolog.Operator

import qualified Data.Map as Map

builtinOpData :: OpData
builtinOpData = mkOpData (Map.fromList binaryOperators)
                         (Map.fromList prefixOperators)
                         (Map.fromList suffixOperators)
  where
    binaryOperators = filter (operOneOf [Xfx, Xfy, Yfx]) operators
    prefixOperators = filter (operOneOf [Fx, Fy]) operators
    suffixOperators = filter (operOneOf [Xf, Yf]) operators

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

    operOneOf opTypes (_, Operator _ _ opTyp) = opTyp `elem` opTypes
    makeOperators (prec, opType, atoms) = map (\name -> (name, Operator name prec opType)) atoms
