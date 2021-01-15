module Assem.Type where


import           Temp.Type

data Instr =
  Oper String [Temp] [Temp] (Maybe [Label])
  | ILabel String Label
  | IMove String Temp Temp
  deriving Show
