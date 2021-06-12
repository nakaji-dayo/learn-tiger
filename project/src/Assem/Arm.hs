{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Assem.Arm where

import           Assem
import           Assem.Type
import           Data.List       (intercalate)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import           Debug.Trace
import           Frame           (TempMap)
import           Frame.Arm
import           Temp            (mkTemp)
import qualified Temp.Type       as Temp
import           Text.Assem
import           Tree

formatLabel (Temp.Label (Just s)) = s

lb (Temp.Label (Just s)) = RawAssem [ALbl s]
num :: Int -> RawAssem
num x = RawAssem [ANum x]

calldefs = []

munchExp :: GenC m => Exp -> m Temp
-- todo: 戻り値つかていない場合のtが消えるか？
munchExp (Call (Name lbl) args) = do
  ats <- mapM munchArgs $ zip [0..] args
  emit $ Oper ("bl " <> lb lbl) ats calldefs Nothing
  -- r0 <- mkTemp
  -- emit $ Oper "`mov `d0 `r0" [r0] [t] Nothing -- todo r0
  pure tempA0
-- -> todo1
munchExp (BinOp Plus l r) = result $ \t -> do
  tl <- munchExp l
  tr <- munchExp r
  emit $ Oper "add `d0 , `s0 , `s1" [tl, tr] [t] Nothing
munchExp (Const i) = result $ \t ->
  emit $ Oper ("mov `d0 , " <> num i) [] [t] Nothing
-- todo1? 後で転送が削除される？
munchExp (Name lbl) = result $ \t ->
  emit $ Oper ("ldr `d0 ," <> lb lbl) [] [t] Nothing
munchExp (Temp t) = pure t
munchExp e = error $ "munchExp: " <> show e

munchStm :: GenC m => Stm -> m ()
munchStm (CJump op le re tl fl) = do
  lt <- munchExp le
  rt <- munchExp re
  emit $ Oper "cmp `s0 , `s1" [lt, rt] [] Nothing
  emit $ Oper ("bne " <> lb fl) [] [] (Just [tl, fl])
munchStm (Label lbl) = emit $ ILabel (formatLabel lbl) lbl
-- TODO1: srcにreg以外を指定できるアドレッシング・モード
munchStm (Move (Temp t) e2) = do
  e2' <- munchExp e2
  emit $ Oper "mov `d0 , `s0" [e2'] [t] Nothing
munchStm (Exp (Call (Name lbl) args)) = do
  ats <- mapM munchArgs $ zip [0..] args
  emit $ Oper ("bl " <> lb lbl) ats calldefs Nothing
munchStm (Jump (Name lbl) ls) =
  emit $ Oper ("b " <> lb lbl) [] [] (Just ls)
munchStm (Exp e) = munchExp e >> pure ()
munchStm s           = error $ "mutchStm: " <> show s

munchArgs (i, e) = do
  t <- munchExp e
  let a = if length argtmps > i then argtmps !! i else error "todo: pass args by frame"
  emit $ Oper "mov `d0 , `s0" [t] [a] Nothing
  pure a


format :: TempMap -> [Instr] -> String
format tm = (<> "\n") . intercalate "\n" . fmap f
  where
    f (Oper a src dst jmp) = "  " <> formatOper tm src dst (fromMaybe [] jmp) a
    f (ILabel s _ )        = s <> ":"
    f IMove {}             = error "todo format IMove"

formatOper :: M.Map Temp.Temp String
           -> [Temp.Temp]
           -> [Temp.Temp]
           -> [Temp.Label]
           -> RawAssem
           -> [Char]
formatOper tm srcs dsts jmps (RawAssem a) = unwords $ map f a
  where
    f (AString s) = s
    f (ANum n)    = '#':show n
    f (ASrc i)    = findtm (srcs ! i)
    f (ADst i)    = findtm (dsts ! i)
    f (AJmp i)    = formatLabel (jmps ! i)
    f (ALbl l)    = l
    xs ! i
      | length xs > i = xs !! i
      | otherwise = error $ "formatOper: " <> show (a, xs, i)
    findtm k = M.findWithDefault (error $ "tm: " <> show (tm, k)) k tm
