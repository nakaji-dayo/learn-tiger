module Temp where

data Temp = Temp Int
  deriving (Show)

data Label = Label (Maybe String)
  deriving (Show)

mkTemp :: Temp
mkTemp = undefined

mkLabel :: Label
mkLabel = undefined

namedLabel :: String -> Label
namedLabel = undefined
