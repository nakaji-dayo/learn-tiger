module Tree where


import           Temp

data Exp =
  Const Int
  | Name Label
  | Temp Temp.Temp
  | BinOp BinOp Exp Exp
  | Mem Exp
  | Call Exp
  | ESeq Stm Exp
  deriving (Show)

data Stm =
  Move Exp Exp
  | Exp Exp
  | Jump Exp [Label]
  | CJump RelOp Exp Exp Label Label
  | Seq Stm Stm
  | Label Label
  deriving (Show)

data BinOp =
  Plus
  | Minus
  | Mul
  | Div
  | And
  | Or
  | LShift
  | RShift
  | ARShift
  | Xor
  deriving (Show)

data RelOp =
  Eq
  | NE
  | LT
  | GT
  | LTE
  | GTE
  | ULT
  | UGT
  | ULTE
  | UGTE
  deriving (Show)
