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
  { frags        :: [Frag f]
  , tempCounter  :: IORef Int
  , labelCounter :: Int
  , instrs :: [Instr]
  }
  deriving (Generic)

newtype GenM f a = GenM (ReaderT (Ctx f) IO a)
  deriving (Functor, Applicative, Monad)
  deriving (HasSource "tempCounter" Int, HasSink "tempCounter" Int, HasState "tempCounter" Int) via ReaderIORef (Field "tempCounter" () (MonadReader (ReaderT (Ctx f) IO)))

type GenC m = (HasState "tempCounter" Int m, HasState "instrs" [Instr] m)

-- todo: s0とか置換やめる
munchExp :: GenC m => Exp -> m Temp
-- -> todo1
munchExp (Const i) = do
  t <- mkTemp
  emit $ Oper ("`mov `d0 " <> formatNum i) [] [t] Nothing
  pure t

-- IF -> CJump -> Assemに無駄あるかも?

emit :: HasState "instrs" [Instr] m => Instr -> m ()
emit x = modify @"instrs" (x :)

munchStm :: GenC m => Stm -> m ()
munchStm (Label lbl) = emit $ ILabel (formatLabel lbl) lbl
-- TODO1: srcにreg以外を指定できるアドレッシング・モード
munchStm (Move (Temp t) e2) = do
  e2' <- munchExp e2
  emit $ Oper "`d0 <- `s0 + r0" [e2'] [t] Nothing
munchStm _           = error "mutchStm"

codegen :: GenC m => f -> Stm -> m [Instr]
codegen f stm = do
  munchStm stm
  get @"instrs"

formatLabel (Temp.Label (Just s)) = s <> ":"
formatNum :: Int -> String
formatNum x = "#" <> show x
