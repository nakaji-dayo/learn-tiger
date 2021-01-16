{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Liveness where


import           Control.Monad
import           Control.Monad.ST
import           Data.Array          (assocs, elems, (!))
import           Data.Array.ST
import           Data.Graph
import           Data.List           (nub)
import qualified Data.Map.Strict     as M
import qualified Data.Set            as S
import           Debug.Pretty.Simple (pTraceShow)
import           Debug.Trace         (trace, traceShow, traceShowM)
import           Graph
import           Temp.Type           (Temp (Temp))

data IGraph = IGraph
  { graph :: Graph
  , tnode :: Temp -> Node
  , gtemp :: Node -> Temp
  , moves :: [(Node, Node)]
  }

-- 「一度に一つの変数」を最適化できないか試したい
--   テンポラリ数に対して走査回数を減らせないか
-- が一旦集合で不動点出す方で実装してみる

interferenceGraph :: FlowGraph -> IGraph
interferenceGraph fg@FlowGraph {..} =
  let b = (0, length def - 1)
      liveMap = liveOut fg
      tmps = S.toList $ S.unions $ elems def
      tnode t = M.fromList (zip tmps [0..]) M.! t
      gtemp g = M.fromList (zip ([0..]::[Int]) tmps) M.! g
      es = nub $ filter (uncurry (/=)) $
        concatMap (\n -> [(tnode d, tnode t) | d <- S.toList (def ! n)
                                             , t <- S.toList (liveMap ! n)])
        [fst b .. snd b]
      graph = buildG (0,length tmps - 1) es
      moves = undefined -- todo: ?定義がわからなかった
  in IGraph {..}

liveOut FlowGraph {..} = runSTArray g
  where
    g :: ST s (STArray s Int (S.Set Temp))
    g = do
      let b = (0, length control - 1)
      let g = newListArray b (repeat S.empty)
      in' <- g
      out' <- g
      let f n = do
            outn <- readArray out' n
            let inn = S.union (use ! n) (S.difference outn (def ! n))
            writeArray in' n inn
            outn <- S.unions <$> mapM (readArray in') (control ! n)
            writeArray out' n outn
      let loop = do
            in'' <- getAssocs in'
            out'' <- getAssocs out'
            mapM_ f $ reverse [fst b .. snd b]
            in''' <- getAssocs in'
            out''' <- getAssocs out'
            when (in'' /= in''' || out'' /= out''') loop
      loop
      pure out'
