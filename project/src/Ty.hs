module Ty where

type Symbol = String

data Ty =
  Int
  | String
  | Record [(Symbol, Ty)]
  | Array Ty
  | Nil
  | Unit
  | Name Symbol
  deriving (Show, Eq)
