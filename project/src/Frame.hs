{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Frame where

import           Control.Monad.RWS (MonadState)
import           Temp.Type
import           Translate.Type
import qualified Tree              as T

class Frame a where
  type Access a = r | r -> a
  newFrame :: Label -> [Bool] -> a
  name :: a -> Label
  formals :: a -> [Access a]
  allocLocal :: MonadState (TransResult a) m => a -> Bool -> m (Access a)
  fp :: Access a -> Temp
  rv :: Access a -> Temp
  wordSize :: Access a -> Int
  exp :: Access a -> T.Exp -> T.Exp
