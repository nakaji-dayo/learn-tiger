{-# LANGUAGE RecordWildCards #-}
module Spec.Graph where

import           Assem.Type
import           Control.Monad
import           Data.Array         ((!))
import           Graph
import           Temp.Type
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Pretty.Simple (pPrint)

unit_instrs2graph_1 = do
  let
    a = Temp 0
    b = Temp 1
    c = Temp 2
    l1 = Label (Just "l1")
    l2 = Label (Just "l2")
    (FlowGraph{..}, vs) = instrs2graph
      [ ILabel "l1" l1
      , Oper "a := 0" [] [a] Nothing
      , Oper "b := a+1" [a] [b] Nothing
      , Oper "c := c+b" [b, b] [c] Nothing
      , Oper "a := b*2" [b] [a] Nothing
      , Oper "a<N" [a] [] (Just [l1, l2])
      , ILabel "l2" l2
      , Oper "return c" [c] [] Nothing
      ]
  print control
  forM_ ([0..4] <> [6, 7]) $ \i -> control ! i @?= []
  control ! 5 @?= [6, 0]
