{-# LANGUAGE RecordWildCards #-}
module Spec.Liveness where

import           Assem.Type
import           Control.Monad
import           Data.Array         ((!))
import qualified Data.Set           as S
import           Graph
import           Liveness
import           Spec.Graph         (smplInstrs)
import           Temp.Type
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Pretty.Simple (pPrint)

unit_liveOut_1 = do
  -- todo Graphから切り離し単体に
  let (fg, vs) = instrs2graph smplInstrs
  let t = liveOut fg
  t ! 0 @?= S.fromList [Temp 0, Temp 2]
  t ! 2 @?= S.fromList [Temp 1,Temp 2]
  t ! 3 @?= S.fromList [Temp 1,Temp 2]
  t ! 5 @?= S.fromList [Temp 0,Temp 2]
  t ! 7 @?= S.fromList []
  pure ()


unit_interferenceGraph_1 = do
  let (fg, vs) = instrs2graph smplInstrs
  let IGraph{..} = interferenceGraph fg
  graph ! 0 @?= [2]
  graph ! 1 @?= [2]
  graph ! 2 @?= [1,0]
