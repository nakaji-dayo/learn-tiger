{-# LANGUAGE RecordWildCards #-}
module Graph where

-- p206のモジュール製の話は怪しいが、 一旦従って実装する

import           Assem.Type  (Instr (ILabel, IMove, Oper))
import           Data.Array
import           Data.Graph
import           Data.List   (findIndex)
import           Data.Maybe  (fromMaybe)
import qualified Data.Set    as S
import           Debug.Trace (traceId, traceShowId)
import           Temp.Type   (Temp (Temp))

type Node = Vertex

data FlowGraph = FlowGraph
  { control :: Graph
  , def     :: Array Node (S.Set Temp)
  , use     :: Array Node (S.Set Temp)
  , ismove  :: Array Node Bool
  }
  deriving (Show)

instrs2graph :: [Instr] -> (FlowGraph, [Node])
instrs2graph is =
  let
    b = (0, length is - 1)
    control = buildG b $ concatMap toedges (zip [0..] is)
    def = listArray b (fmap (S.fromList . todefs) is)
    use = listArray b (fmap (S.fromList . touses) is)
    ismove = listArray b (fmap toismv is)
  in (FlowGraph {..}, [])
  where
    findLabel l =
      let p (ILabel _ l') = l' == l
          p _             = False
      in fromMaybe (error $ "undefined label: " <> show l) $ findIndex p is
    toedges :: (Int,  Instr) -> [(Node, Node)]
    toedges (n, Oper _ _ _ (Just js)) = fmap (\l -> (n, findLabel l)) js
    toedges (n, _)
      | length is > n+1 = [(n, n+1)]
      | otherwise = []
    todefs (Oper _ _ xs _) = xs
    todefs _               = []
    touses (Oper _ xs _ _) = xs
    touses _               = []
    toismv IMove {} = True
    toismv _        = False
