{-# LANGUAGE RecordWildCards #-}
module Color where

import           Data.Array
import           Data.Foldable   (foldl', foldlM)
import           Data.Graph
import           Data.List       (find)
import qualified Data.Map.Strict as M
import           Data.Maybe      (mapMaybe)
import           Debug.Trace     (trace, traceShow)
import           Frame           (Register, TempMap)
import           Graph           (Node)
import           Liveness
import           Temp.Type       (Temp (NamedTemp, Temp))
import GHC.Stack

type Allocation = TempMap

color :: HasCallStack => IGraph -> Allocation -> [Register] -> (Allocation, [Temp])
color IGraph {..} init rs  =
  let r = loop
  in (M.union r init, [])
  where
    rlen = length rs
    isPrecolored (NamedTemp _) = True
    isPrecolored _             = False
    assocs' rms = filter (\(k, _) -> k `notElem` rms) $ assocs graph
    vertices' rms = filter (`notElem` rms) $ vertices graph
    allPrecolored rms g = all isPrecolored $ gtemp <$> vertices' rms
    simplify selectStack =
      let
        test x = not (isNamed (gtemp $ fst x))
          && length (filter (`notElem` selectStack) $ snd x) < rlen
        rm = filter test $ assocs' selectStack
      in fmap fst rm <> selectStack
    select :: M.Map Node Register -> Node -> M.Map Node Register
    select m n =
      let adcs = mapMaybe (`M.lookup` m) $ graph ! n
          mc = find (`notElem` adcs) rs
      in case mc of
        Just c  -> M.insert n c m
        Nothing -> error "Error: select color"
    loop =
      let f1 sel
            | allPrecolored sel graph  = sel
            | otherwise =
              let sel' = simplify sel
              in if sel == sel'
                 then error $ "todo Spill: interferences: " <> show (gtemp <$> vertices' sel)
                 else f1 sel'
          simplified = f1 []
          init' = M.mapKeys tnode init
          selected = M.mapKeys gtemp $ foldl' select M.empty simplified
      in selected


isNamed (NamedTemp _) = True
isNamed _             = False

-- todo: spill, coalesced
--   特に合併は早く実装したい
