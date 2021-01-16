{-# LANGUAGE RecordWildCards #-}
module Graph where

-- p206のモジュール製の話は怪しいが、 一旦従って実装する

import           Assem.Type  (Instr (ILabel, IMove, Oper))
import           Data.Array
import           Data.Graph
import           Data.List   (findIndex)
import           Data.Maybe  (fromMaybe)
import           Debug.Trace (traceId, traceShowId)
import           Temp.Type   (Temp (Temp))

data FlowGraph = FlowGraph
  { control :: Graph
  , def     :: Array Vertex [Temp]
  , use     :: Array Vertex [Temp]
  , ismove  :: Array Vertex Bool
  }
  deriving (Show)

instrs2graph :: [Instr] -> (FlowGraph, [Vertex])
instrs2graph is =
  let
    b = (0, length is - 1)
    control = buildG b $ concatMap toedges (zip [0..] is)
    def = listArray b (fmap todefs is)
    use = listArray b (fmap touses is)
    ismove = listArray b (fmap toismv is)
  in (FlowGraph {..}, [])
  where
    findLabel l =
      let p (ILabel _ l') = l' == l
          p _             = False
      in fromMaybe (error $ "undefined label: " <> show l) $ findIndex p is
    toedges :: (Int,  Instr) -> [(Vertex, Vertex)]
    toedges (n, Oper _ _ _ (Just js)) = fmap (\l -> (n, findLabel l)) js
    toedges _                         = []
    todefs (Oper _ xs _ _) = xs
    todefs _               = []
    touses (Oper _ _ xs _) = xs
    touses _               = []
    toismv IMove {} = True
    toismv _        = False
