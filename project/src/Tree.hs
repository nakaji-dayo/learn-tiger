module Tree where


import           Temp.Type as T

data Exp =
  Const Int
  | Name Label
  | Temp T.Temp
  | BinOp BinOp Exp Exp
  | Mem Exp
  | Call Exp [Exp]
  | ESeq Stm Exp
  deriving (Show, Eq)

data Stm =
  Move Exp Exp
  | Exp Exp
  | Jump Exp [Label]
  | CJump RelOp Exp Exp Label Label
  | Seq Stm Stm
  | Label Label
  deriving (Show, Eq)

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
  deriving (Show, Eq)

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
  deriving (Show, Eq)
