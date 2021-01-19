{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Frame where

import           Assem.Type        (Instr)
import           Capability.State  (HasState)
import           Control.Monad.RWS (MonadState)
import qualified Data.Map          as M
import           Temp.Type
import           Translate.Type
import qualified Tree              as T

type Register = String
type TempMap = M.Map Temp Register

class Frame a where
  type Access a = r | r -> a
  newFrame :: Label -> [Bool] -> a
  name :: a -> Label
  formals :: a -> [Access a]
  allocLocal :: HasState "tempCounter" Int m => a -> Bool -> m (Access a)
  fp :: Access a -> Temp
  rv :: Access a -> Temp
  wordSize :: Access a -> Int
  exp :: Access a -> T.Exp -> T.Exp
  -- クラス分ける?
  --   add Machine class, rename HasFrame
  registers :: a -> [Register]
  tempMap :: a -> TempMap
  procEntryExit2 :: a -> [Instr] -> [Instr]
