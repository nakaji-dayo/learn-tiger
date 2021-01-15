{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Assem where

import           Frame               (Register, Frame)
import           Temp                (mkTemp)
import qualified Temp.Type           as Temp
import Tree
    ( BinOp(Plus),
      Exp(Name, BinOp, Const, Temp, Call),
      Stm(Exp, CJump, Label, Move, Jump) )
import Translate.Type ()
import Control.Monad.Reader (ReaderT(..))
import Capability.Reader ( Field(..), MonadReader(..) )
import Capability.State
    ( get, modify, put, HasState, ReaderIORef(..) )
import GHC.Generics (Generic)
import Data.IORef ( newIORef, IORef )
import Capability.Source ( HasSource )
import Capability.Sink ( HasSink )
import Type (TransC)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad (void)
import Assem.Type

type Reg = String
type Temp = Temp.Temp
type Label = Temp.Label

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

runCodegen :: (TransC f m, MonadIO m) => (Stm -> GenM f1 b) -> f -> [Stm] -> m [Instr]
runCodegen munch f stm = do
  tc <- get @"tempCounter" >>= liftIO . newIORef
  lc <- get @"labelCounter" >>= liftIO . newIORef
  is <- liftIO $ newIORef []
  (instrs, tc', lc') <- liftIO $ runReaderT (unGenM (codegen stm)) (Ctx tc lc is)
  put @"tempCounter" tc'
  put @"labelCounter" lc'
  pure instrs
  where
    codegen stm = do
      mapM_ munch stm
      (,,) <$> (reverse <$> get @"instrs") <*> get @"tempCounter" <*> get @"labelCounter"
