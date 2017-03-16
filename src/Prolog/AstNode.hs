module Prolog.AstNode ( AstNode(..) ) where

data AstNode = Atom String
             | Var String
             | PInt Integer
             | PFloat Double
             | Str String
             | Comp String [AstNode]

join :: String -> [String] -> String
join delim strs = concat . tail $ zipWith (++) (repeat delim) strs

instance Eq AstNode where
  Atom a        == Atom b        = a == b
  Var a         == Var b         = a == b
  PInt a        == PInt b        = a == b
  PFloat a      == PFloat b      = a == b
  Str a         == Str b         = a == b
  Comp p0 args0 == Comp p1 args1 = (p0, args0) == (p1, args1)
  _             == _             = False

instance Show AstNode where
  show (Atom a)            = "<Atom " ++ show a ++ ">"
  show (Var v)             = "<Var " ++ show v ++ ">"
  show (PInt i)            = "<PInt " ++ show i ++ ">"
  show (PFloat f)          = "<PFloat " ++ show f ++ ">"
  show (Str s)             = "<Str " ++ s ++ ">"
  show (Comp proc args)    = join " " ("<Comp" : proc : map show args) ++ ">"
