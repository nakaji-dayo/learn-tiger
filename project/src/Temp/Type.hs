module Temp.Type where


data Temp = Temp Int | NamedTemp String
  deriving (Show)

data Label = Label (Maybe String)
  deriving (Show)
