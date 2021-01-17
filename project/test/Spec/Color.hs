{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module Spec.Color where

import           Assem.Type
import           Color
import           Control.Monad
import           Data.Array         ((!))
import           Data.Bifunctor     (bimap)
import           Data.Graph
import           Data.List          (elemIndex)
import qualified Data.Map.Strict    as M
import           Data.Maybe         (fromJust)
import qualified Data.Set           as S
import           Graph
import           Liveness
import           Temp.Type
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Pretty.Simple (pPrint)

sGraph =
  let
    rs@(b:c:d:e:f:g:h:j:k:m:_) = fmap Temp [0..9]
    tnode t = fromJust $ elemIndex t rs
    es = bimap tnode tnode <$>
      concat [ (b,) <$> [d,c,e,m,k]
      , (c,) <$> [b,m]
      , (d,) <$> [j,k,b,m]
      , (e,) <$> [f,j,b,m]
      , (f,) <$> [j,e,m]
      , (g,) <$> [j,h,k]
      , (h,) <$> [j,g]
      , (j,) <$> [f,e,k,d,h,g]
      , (k,) <$> [j,g,d,b]
      , (m,) <$> [e,f,b,c,d]
      ]
    gtemp = (rs !!)
    graph = buildG (0, 9) es
    moves = undefined
  in IGraph {..}

unit_color :: IO ()
unit_color = do
  let IGraph {..} = sGraph
  let (r, _) = color sGraph M.empty ["r0", "r1", "r2", "r3"]
  forM_ [0..9] $ \i ->
                   let adcs = fmap ((r M.!) . gtemp) $ graph ! i
                       c = r M.! gtemp i
                   in if c `elem` adcs
                      then assertFailure "conflict"
                      else print (i, c, adcs)
