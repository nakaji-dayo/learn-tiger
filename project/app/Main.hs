module Main where

import           Lib
import           System.Environment

main :: IO ()
main = do
  f:out:_ <- getArgs
  file f out
