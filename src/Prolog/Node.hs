module Prolog.Node ( Node(..) ) where


data Node = Atom String
             | Var String
             | PInt Integer
             | PFloat Double
             | Str String
             | Nil
             | Func String [Node]

join :: String -> [String] -> String
join _ [] = ""
join delim (x:xs) = concat  $ x : zipWith (++) (repeat delim) xs

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
  show (Atom a)            = "(Atom " ++ show a ++ ")"
  show (Var v)             = "(Var " ++ show v ++ ")"
  show (PInt i)            = "(PInt " ++ show i ++ ")"
  show (PFloat f)          = "(PFloat " ++ show f ++ ")"
  show (Str s)             = "(Str " ++ s ++ ")"
  show Nil                 = "[]"
  show func@(Func proc args)
    | isPList func = "[" ++ show (head args) ++ showCdr (head $ tail args)
    | otherwise = join " " ("(Func" : proc : map show args) ++ ")"
    where isPList (Func proc args) = proc == "[|]" && length args == 2
          isPList _ = False

          showCdr Nil = "]"
          showCdr v@(Func proc [h, l])
            | isPList v = ", " ++ show h ++ showCdr l
          showCdr v = " | " ++ show v ++ "]"
