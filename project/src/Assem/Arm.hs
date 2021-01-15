{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Assem.Arm where

import           Assem
import           Assem.Type
import           Frame.Arm
import           Temp       (mkTemp)
import qualified Temp.Type  as Temp
import           Tree

formatLabel (Temp.Label (Just s)) = s
formatNum :: Int -> String
formatNum x = "#" <> show x

calldefs = []

munchExp :: GenC m => Exp -> m Temp
-- todo: 戻り値つかていない場合のtが消えるか？
munchExp (Call (Name lbl) args) = do
  ats <- mapM munchArgs $ zip [0..] args
  emit $ Oper ("bl " <> formatLabel lbl) ats calldefs Nothing
  -- r0 <- mkTemp
  -- emit $ Oper "`mov `d0 `r0" [r0] [t] Nothing -- todo r0
  pure tempA0
-- -> todo1
munchExp (BinOp Plus l r) = result $ \t -> do
  tl <- munchExp l
  tr <- munchExp r
  emit $ Oper "add `d0 `s0" [tl, tr] [t] Nothing
munchExp (Const i) = result $ \t ->
  emit $ Oper ("`mov `d0 " <> formatNum i) [] [t] Nothing
-- todo1? 後で転送が削除される？
munchExp (Name lbl) = result $ \t ->
  emit $ Oper ("ldr t " <> formatLabel lbl) [] [t] Nothing
munchExp (Temp t) = pure t
munchExp e = error $ "munchExp: " <> show e

munchStm :: GenC m => Stm -> m ()
munchStm (CJump op le re tl fl) = do
  lt <- munchExp le
  rt <- munchExp le
  emit $ Oper "cmp `s0 `s1" [lt, rt] [] Nothing
  emit $ Oper ("bne " <> formatLabel tl) [] [] (Just [tl, fl])
munchStm (Label lbl) = emit $ ILabel (formatLabel lbl) lbl
-- TODO1: srcにreg以外を指定できるアドレッシング・モード
munchStm (Move (Temp t) e2) = do
  e2' <- munchExp e2
  emit $ Oper "`mov `d0 `s0" [e2'] [t] Nothing
munchStm (Exp (Call (Name lbl) args)) = do
  ats <- mapM munchArgs $ zip [0..] args
  emit $ Oper ("bl " <> formatLabel lbl) ats calldefs Nothing
munchStm (Jump (Name lbl) ls) =
  emit $ Oper ("b " <> formatLabel lbl) [] [] (Just ls)
munchStm (Exp e) = munchExp e >> pure ()
munchStm s           = error $ "mutchStm: " <> show s

munchArgs (i, e) = do
  t <- munchExp e
  let a = if length argregs > i then argregs !! i else error "todo: pass args by frame"
  emit $ Oper "mv `d0 `s0" [t] [a] Nothing
  pure a
