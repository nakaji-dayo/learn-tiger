{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeApplications      #-}

module NFA where

import           Control.Lens    ((^.))
import           Control.Monad   (msum, (>>))
import           Data.Foldable   (toList)
import           Data.List       (findIndex, nubBy)
import qualified Data.Map.Strict as M
import           Data.Maybe      (catMaybes, fromMaybe)
import           Data.Monoid
import           Data.Sequence   (Seq (..), findIndexL, fromList, index, (|>))
import           Debug.Trace
import           GHC.Records     (HasField (getField))



anyChar = ['0'..'z']
lower = ['a'..'z']
digit = ['0'..'9']

data Node = Node
  { symbol :: Int
  , final  :: Maybe String
  } deriving (Show, Eq)

data State a = State
  { node  :: a
  , edges :: [([Char], State a)]
  } deriving (Show) --

-- foldMapOnce :: (Eq a, Monoid m) => (a -> m) -> State a -> m
-- foldMapOnce f (State n s) =
--   go [] f (State n s)
--   where
--     go used f (State n s)
--       | n `elem` used = mempty
--       | otherwise = f n <> mconcat (go (n:used) f . snd <$> s)

-- findOnce :: (Eq a) => (a -> Bool) -> State a -> Maybe a
-- findOnce p = getFirst . foldMapOnce (\ x -> First (if p x then Just x else Nothing))

findStateBy :: Eq a => (a -> Bool) -> State a -> Maybe (Maybe [Char], State a)
findStateBy f s = go Nothing [] s
  where
    go c used s@(State n' es)
      | n' `elem` used = Nothing
      | f n' = Just (c, s)
      | otherwise =
          case es of
            [] -> Nothing
            _  -> msum $ map (\(c, s) -> go (Just c) (n':used) s) es

nfaState s mf es = State (Node s mf) es

s8 = nfaState 8 (Just "ID")
     [ ("", nfaState 6 Nothing
            [ ((lower ++ digit), nfaState 7 Nothing
                                 [ ("", s8)
                                 ]
              )
            ]
       )
     ]
s13 = nfaState 13 (Just "NUM")
     [ ("", nfaState 11 Nothing
            [ (digit, nfaState 12 Nothing
                                 [ ("", s13)
                                 ]
              )
            ]
       )
     ]
r = nfaState 1 Nothing
    [ ("i", nfaState 2 Nothing
            [ ("f", nfaState 3 (Just "IF") [])
            ]
      )
    , ("", nfaState 14 Nothing
           [ (anyChar, nfaState 15 (Just "ERROR") [])
           ]
      )
    , ("", nfaState 4 Nothing
           [ (lower, nfaState 5 Nothing
               [ ("", s8
                 )
               ]
             )
           ]
      )
    , ("", nfaState 9 Nothing
           [ (digit, nfaState 10 Nothing
               [ ("", s13
                 )
               ]
             )
           ]
      )
    ]


pp :: (Show a, Show b, Eq b, HasField "symbol" a b) => State a -> IO ()
pp = pp' [] 0 ""
  where
    pp' used lv c (State n es)
      | getField @"symbol" n `elem` used = putStrLn $ concat [take (lv*2) $ repeat ' ', "(", show $ getField @"symbol" n, ")"]
      | otherwise = do
          putStrLn $ concat [take (lv*2) $ repeat ' ', "[", show $ getField @"symbol" n, "]", ps c, " ==> "]
          mapM_ (uncurry (pp' (getField @"symbol" n:used) (lv + 1))) es
      where
        ps "" = "ε"
        ps x  = x

showSymbols :: Show a => [State a] -> IO ()
showSymbols = mapM_ (print . node)

-- Nothing: e
edge c s =
  map snd $ filter (match c) $ edges s
  where
    match Nothing ("", _) = True
    match (Just c) (x, _) = c `elem` x
    match _ _             = False

closure :: Eq a => [State a] -> [State a]
closure s =
  let t = nubBy (\a b -> node a == node b) $ s ++ (concat $ edge Nothing <$> s)
  in if eqSymbols t s
     then t
     else closure t

eqSymbols xs ys = map node xs == map node ys

dfaEdge :: Eq a => Char -> [State a] -> [State a]
dfaEdge c s =
  closure $ concatMap (edge (Just c)) s

runNFA cs s = go cs (closure [s])
  where
    go [] s'     = s'
    go (c:cs) s' = go cs (dfaEdge c s')

----


data DFANode = DFANode
  { symbol :: [Int]
  , final  :: Maybe String
  } deriving (Show, Eq)

--  NFA -> DFA変換
-- 写経。直接State DFANodeにreduceできる？

type Intermediate = (Seq [State Node], [(Int, Char, Int)])

trans s =
  step (fromList [closure [s]], []) 0 0
  where
    step :: Intermediate -> Int -> Int -> Intermediate
    step x j p
      | j > p = x
      | otherwise =
        let (x', j', p') = foldl (\acc x -> go x acc) (x, j, p) anyChar
        in step x' (j' + 1) p'
    go :: Char -> (Intermediate, Int, Int) -> (Intermediate, Int, Int)
    go c ((state, trans), j, p) =
      let e = dfaEdge c (state `index` j)
      in case findIndexL (shallowEq $ e) state of
        Just idx ->
          ((state, (j, c, idx):trans), j, p)
        Nothing ->
          ((state |> e, (j, c, p+1):trans), j, p+1)
    shallowEq x y = map (getField @"symbol" . node) x == map (getField @"symbol" . node) y

reduceTrans xs = M.fromListWith (++) $ map (\(f, c, t) -> ((f, t), [c])) xs

test (state, trans) = do
  mapM_ (\(i, s) -> print i >> showSymbols s) $ zip [0..] (toList state)
  mapM_ print (M.toList $ reduceTrans trans)

ra =
  let s1 = nfaState 1 Nothing [("", s2), ("x", s5)]
      s2 = nfaState 2 Nothing [("", s3), ("y", s6)]
      s3 = nfaState 3 Nothing [("", s4)]
      s4 = nfaState 4 Nothing [("", s1)]
      s5 = nfaState 5 Nothing [("z", s2), ("", s6)]
      s6 = nfaState 6 Nothing [("", s7)]
      s7 = nfaState 7 (Just "F") []
  in s1
