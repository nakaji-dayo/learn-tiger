{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Assem.Type where


import           Data.String
import           Temp.Type
import           Text.Assem



data Instr =
  Oper RawAssem [Temp] [Temp] (Maybe [Label])
  | ILabel String Label
  | IMove String Temp Temp
  deriving Show
