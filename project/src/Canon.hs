{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Strict #-}

module Canon where

import           Tree
import qualified Temp.Type as Temp
import Control.Monad.State (runState, StateT(runStateT), MonadState)
import Translate.Type
import Temp (mkLabel, mkTemp)
import Data.Foldable (foldlM)
import Debug.Trace (traceM)
import Capability.State

type CanonC m = ( HasState "tempCounter" Int m
                  , HasState "labelCounter" Int m
                  )


linearize :: Stm -> [Stm]
linearize stm0 = linear [] (doStm stm0)
  where
    lseq (Exp (Const _)) x = x
    lseq x (Exp (Const _)) = x
    lseq x y = Seq x y

    commute :: Stm -> Exp -> Bool
    commute (Exp (Const _)) _ = True
    commute _ (Name _)        = True
    commute _ (Const _)       = True
    commute _ _               = False

    -- [Seq(s1, Seq(s1,...), ...] -> [s1, s2, ...]
    linear :: [Stm] -> Stm -> [Stm]
    linear l (Seq x y) = linear (linear l y) x
    linear l s         = s:l

    reorder :: [Exp] -> (Stm, [Exp])
    -- todo: CALL
    reorder (x:xs) =
      let (stms, e) = doExp x
          (stms', el) = reorder xs
      in if commute stms' e then (lseq stms stms' , e:el) else (
           let t = Temp.Temp 999
           in (lseq (lseq stms (Move (Temp t) e)) stms', Temp t:el))
    reorder [] = (Exp $ Const 0, [])


    reorderStm :: [Exp] -> ([Exp] -> Stm) -> Stm
    reorderStm el build =
      let (stms, el') = reorder el
      in lseq stms (build el')
    reorderExp el build =
      let (stms, el') = reorder el
      in (stms, build el')

    doStm (Seq a b) = lseq (doStm a) (doStm b)
    doStm (Jump e ls) = reorderStm [e] \(e:_) -> Jump e ls
    doStm (CJump p a b t f) = reorderStm [a, b] \(a:b:_) -> CJump p a b t f
    doStm (Move (Temp t) (Call e el)) = reorderStm (e:el) \(e:el) -> Move (Temp t) (Call e el)
    doStm (Move (Temp t) b) = reorderStm [b] \[b] -> Move (Temp t) b
    doStm (Move (Mem e) b) = reorderStm [e, b] \[e, b] -> Move (Mem e) b
    doStm (Move (ESeq s e) b) =  doStm $ lseq s (Move e b) -- tail1
    doStm (Exp (Call e el)) = reorderStm (e:el) \(e:el) -> Exp (Call e el)
    doStm (Exp e) = reorderStm [e] \(e:_) ->  Exp e
    doStm s = reorderStm [] (const s)

    doExp (BinOp p a b) = reorderExp [a, b] \(a:b:_) -> BinOp p a b
    doExp (Mem a) = reorderExp [a] \(a:_) -> Mem a
    doExp (ESeq s e) =
      let stms = doStm s
          (stms', e') = doExp e
      in (lseq stms stms', e')
    doExp (Call e el) = reorderExp (e:el) \(e:el) -> Call e el
    doExp e = reorderExp [] (const e)


basicBlocks :: CanonC m => [Stm] -> m ([[Stm]], Temp.Label)
basicBlocks stms = do
  done <- mkLabel
  start <- mkLabel
  (lc, bs) <- foldlM f ([], []) stms
  pure (reverse (reverse lc:bs), done)
  where
    f ([], acc) x@(Label _) = pure ([x], acc)
    f ([], acc) x = do  -- labelから始まっていない
      l <- mkLabel
      f ([Label l], acc) x
    f (c, acc) x@(Label lbl) = -- jumpで終了せずlabelにあたった
      let next = Jump (Name lbl) [lbl]
      in pure ([x], (next:c):acc)
    f (c, acc) x@(Jump _ _) =
      let b = reverse (x:c)
      in pure ([], b:acc)
    f (c, acc) x@CJump {} =
      let b = reverse $ x:c
      in pure ([], b:acc)
    f (c, acc) x = pure (x:c, acc)

-- todo
-- cjumpのf labelが飛ぶ実装が追加されてから
-- trace

traceSchedule :: ([[Stm]], Temp.Label) -> [Stm]
traceSchedule = undefined


runCanon :: CanonC m => Stm -> m ([Stm], ([[Stm]], Temp.Label))
runCanon stm = do
      let stms = linearize stm
      stms' <- basicBlocks stms
      pure (stms, stms')
