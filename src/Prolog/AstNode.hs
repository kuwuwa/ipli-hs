module Prolog.AstNode ( AstNode(..) ) where


data AstNode = Atom String
             | Var String
             | PInt Integer
             | PFloat Double
             | Str String
             | Func String [AstNode]
             | Nil
             | Pair AstNode AstNode

join :: String -> [String] -> String
join _ [] = ""
join delim (x:xs) = concat  $ x : zipWith (++) (repeat delim) xs

instance Eq AstNode where
  Atom a        == Atom b        = a == b
  Var a         == Var b         = a == b
  PInt a        == PInt b        = a == b
  PFloat a      == PFloat b      = a == b
  Str a         == Str b         = a == b
  Func p0 args0 == Func p1 args1 = (p0, args0) == (p1, args1)
  Nil           == Nil           = True
  Pair h0 t0    == Pair h1 t1    = (h0, t0) == (h1, t1)
  _             == _             = False

instance Show AstNode where
  show (Atom a)            = "(Atom " ++ show a ++ ")"
  show (Var v)             = "(Var " ++ show v ++ ")"
  show (PInt i)            = "(PInt " ++ show i ++ ")"
  show (PFloat f)          = "(PFloat " ++ show f ++ ")"
  show (Str s)             = "(Str " ++ s ++ ")"
  show (Func proc args)    = join " " ("(Func" : proc : map show args) ++ ")"
  show Nil                 = "[]"
  show (Pair h t)          = "[" ++ show h ++ showCdr t
    where showCdr Nil        = "]"
          showCdr (Pair h t) = ", " ++ show h ++ showCdr t
          showCdr v          = " | " ++ show v ++ "]"
