module Token where

data Token = Atom String
           | Var String
           | NumI Integer
           | NumF Double
           | Str String
           | LParen
           | RParen
           | LBracket
           | RBracket

instance Eq Token where
  Atom a == Atom b = a == b
  Var a  == Var b  = a == b
  NumI a == NumI b = a == b
  NumF a == NumF b = a == b
  Str a  == Str b  = a == b
  LParen == LParen = True
  RParen == RParen = True
  _ == _           = False

instance Show Token where
  show (Atom a) = "Atom " ++ show a
  show (Var a)  = "Var "  ++ show a
  show (NumI a) = "NumI " ++ show a
  show (NumF a) = "NumF " ++ show a
  show (Str a) =  "Str "  ++ show a
  show LParen   = "LParen"
  show RParen   = "RParen"
  show LBracket = "LBracket"
  show RBracket = "RBracket"
