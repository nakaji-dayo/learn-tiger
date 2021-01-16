module Temp.Type where


data Temp = Temp Int | NamedTemp String
  deriving (Show, Eq)

data Label = Label (Maybe String)
  deriving (Show, Eq)
