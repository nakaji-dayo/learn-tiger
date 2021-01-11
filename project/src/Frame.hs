{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Frame where

import           Temp
import qualified Tree as T

class Frame a where
  type Access a = r | r -> a
  newFrame :: Label -> [Bool] -> a
  name :: a -> Label
  formals :: a -> [Access a]
  allocLocal :: a -> Bool -> Access a
  fp :: Access a -> Temp
  rv :: Access a -> Temp
  wordSize :: Access a -> Int
  exp :: Access a -> T.Exp -> T.Exp
