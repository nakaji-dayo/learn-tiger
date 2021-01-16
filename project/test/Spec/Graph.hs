{-# LANGUAGE RecordWildCards #-}
module Spec.Graph where

import           Assem.Type
import           Control.Monad
import           Data.Array         ((!))
import qualified Data.Set           as S
import           Graph
import           Temp.Type
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Pretty.Simple (pPrint)

smplInstrs =
  let
    a = Temp 0
    b = Temp 1
    c = Temp 2
    l1 = Label (Just "l1")
    l2 = Label (Just "l2")
  in
    [ Oper "a := 0" [] [a] Nothing
    , ILabel "l1" l1
    , Oper "b := a+1" [a] [b] Nothing
    , Oper "c := c+b" [b, c] [c] Nothing
    , Oper "a := b*2" [b] [a] Nothing
    , Oper "a<N" [a] [] (Just [l1, l2])
    , ILabel "l2" l2
    , Oper "return c" [c] [] Nothing
    ]

unit_instrs2graph_1 = do
  let (FlowGraph{..}, vs) = instrs2graph smplInstrs
  forM_ ([0..4] <> [6]) $ \i -> control ! i @?= [i + 1]
  control ! 5 @?= [6, 1]
  use ! 0 @?= S.fromList []
  def ! 0 @?= S.fromList [Temp 0]
  use ! 3 @?= S.fromList [Temp 1, Temp 2]
  def ! 3 @?= S.fromList [Temp 2]
