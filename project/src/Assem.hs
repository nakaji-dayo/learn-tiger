{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Assem where

import           Frame               (Frame)
import           Temp                (mkTemp)
import qualified Temp.Type           as Temp
import           Tree
import Translate.Type
import Control.Monad.Reader (ReaderT(..))
import Capability.Reader
import Capability.State
import Frame.Arm (ArmFrame(ArmFrame))
import GHC.Generics (Generic)
import Data.IORef
import Capability.Source
import Capability.Sink
import Type (TransC)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad (void)

type Reg = String
type Temp = Temp.Temp
type Label = Temp.Label

data Instr =
  Oper String [Temp] [Temp] (Maybe [Label])
  | ILabel String Label
  | IMove String Temp Temp
  deriving Show

format :: (Temp -> String) -> Instr -> String
format = undefined

data Ctx f = Ctx
  { tempCounter  :: IORef Int
  , labelCounter :: IORef Int
  , instrs :: IORef [Instr]
  }
  deriving (Generic)

newtype GenM f a = GenM {unGenM :: ReaderT (Ctx f) IO a}
  deriving (Functor, Applicative, Monad)
  deriving (HasSource "tempCounter" Int, HasSink "tempCounter" Int, HasState "tempCounter" Int) via ReaderIORef (Field "tempCounter" () (MonadReader (ReaderT (Ctx f) IO)))
  deriving (HasSource "labelCounter" Int, HasSink "labelCounter" Int, HasState "labelCounter" Int) via ReaderIORef (Field "labelCounter" () (MonadReader (ReaderT (Ctx f) IO)))
  deriving (HasSource "instrs" [Instr], HasSink "instrs" [Instr], HasState "instrs" [Instr]) via ReaderIORef (Field "instrs" () (MonadReader (ReaderT (Ctx f) IO)))

type GenC m = (HasState "tempCounter" Int m, HasState "instrs" [Instr] m)

emit :: HasState "instrs" [Instr] m => Instr -> m ()
emit x = modify @"instrs" (x :)

result f = do
  t <- mkTemp
  f t
  pure t
-- todo: s0とか置換やめる
munchExp :: GenC m => Exp -> m Temp
-- todo: 戻り値つかていない場合のtが消えるか？
munchExp (Call (Name lbl) args) = result $ \t -> do
  ats <- mapM munchArgs $ zip [0..] args
  emit $ Oper ("bl " <> formatLabel lbl) ats calldefs Nothing
  r0 <- mkTemp
  emit $ Oper ("`mov `d0 `r0") [r0] [t] Nothing -- todo r0
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
  a <- mkTemp  -- todo use a1~a4
  emit $ Oper "mv `d0 `s0" [t] [a] Nothing
  pure a

runCodegen :: (TransC f m, MonadIO m) => f -> [Stm] -> m [Instr]
runCodegen f stm = do
  tc <- get @"tempCounter" >>= liftIO . newIORef
  lc <- get @"labelCounter" >>= liftIO . newIORef
  is <- liftIO $ newIORef []
  (instrs, tc', lc') <- liftIO $ runReaderT (unGenM (codegen stm)) (Ctx tc lc is)
  put @"tempCounter" tc'
  put @"labelCounter" lc'
  pure instrs
  where
    codegen stm = do
      mapM_ munchStm stm
      (,,) <$> (reverse <$> get @"instrs") <*> get @"tempCounter" <*> get @"labelCounter"

formatLabel (Temp.Label (Just s)) = s
formatNum :: Int -> String
formatNum x = "#" <> show x

calldefs = []
