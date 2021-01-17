{-# LANGUAGE RecordWildCards #-}
module Color where

import           Data.Array
import           Data.Foldable   (foldl', foldlM)
import           Data.Graph
import           Data.List       (find)
import qualified Data.Map.Strict as M
import           Data.Maybe      (mapMaybe)
import           Debug.Trace     (traceShow)
import           Frame           (Register)
import           Liveness
import           Temp.Type       (Temp (NamedTemp, Temp))

type Allocation = M.Map Temp Register

color :: IGraph -> Allocation -> [Register] -> (Allocation, [Temp])
color IGraph {..} init rs  =
  let r = loop
  in (r, [])
  where
    rlen = length rs
    isPrecolored (NamedTemp _) = True
    isPrecolored _             = False
    assocs' rms = filter (\(k, _) -> k `notElem` rms) $ assocs graph
    vertices' rms = filter (`notElem` rms) $ vertices graph
    allPrecolored rms g = all isPrecolored $ gtemp <$> vertices' rms
    simplify selectStack =
      let f = filter (\x -> length (filter (`notElem` selectStack) $ snd x) < rlen) $ assocs' selectStack
      in fmap fst f <> selectStack
    select m n =
      let adcs = mapMaybe (`M.lookup` m) $ graph ! n
          mc = find (`notElem` adcs) rs
      in case mc of
        Just c  -> M.insert n c m
        Nothing -> error "Error: select color"
    loop =
      let f1 sel
            | allPrecolored sel graph  = traceShow ("end", test . gtemp <$> sel) sel
            | otherwise =
              let sel' = simplify sel
              in if sel == sel'
                 then error $ "todo Spill: interferences: " <> show (gtemp <$> vertices' sel)
                 else f1 sel'
          simplified = f1 []
          selected = M.mapKeys gtemp $ foldl' select M.empty simplified
      in traceShow (test . gtemp <$> simplified) selected

test (Temp i) = "bcdefghjkm" !! i

-- todo: spill, coalesced
