module Prolog.Node (
    Node(..)
  ) where

data Node = Atom String
          | Var String
          | PInt Integer
          | PFloat Double
          | Str String
          | Nil
          | Func String [Node]

instance Eq Node where
  Atom a        == Atom b        = a == b
  Var a         == Var b         = a == b
  PInt a        == PInt b        = a == b
  PFloat a      == PFloat b      = a == b
  Str a         == Str b         = a == b
  Nil           == Nil           = True
  Func p0 args0 == Func p1 args1 = (p0, args0) == (p1, args1)
  _             == _             = False

instance Show Node where
  show (Atom a)             = "(Atom " ++ show a ++ ")"
  show (Var v)              = "(Var " ++ show v ++ ")"
  show (PInt i)             = "(PInt " ++ show i ++ ")"
  show (PFloat f)           = "(PFloat " ++ show f ++ ")"
  show (Str s)              = "(Str " ++ s ++ ")"
  show Nil                  = "[]"
  show (Func "[|]" [hd, tl]) = "[" ++ show hd ++ showCdr tl
    where showCdr Nil = "]"
          showCdr (Func "[|]" [h, l]) = ", " ++ show h ++ showCdr l
          showCdr v = " | " ++ show v ++ "]"
  show (Func proc args)     = join " " ("(Func" : proc : map show args) ++ ")"
    where join _ [] = ""
          join delim (x:xs) = concat  $ x : zipWith (++) (repeat delim) xs