{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
module Translate.Type where
import           Capability.Reader
import           Capability.State  (HasState)
import           Temp.Type         (Label (Label))
import qualified Tree              as T

data Frag a = FProc T.Stm a
            | FString Label String

instance Show (Frag a) where
  show (FString l s) = show (l, s)
  show _             = "FProc ? ?"

-- data TransResult a = TransResult
--   { frags        :: [Frag a]
--   , tempCounter  :: Int
--   , labelCounter :: Int
--   }
--   deriving (Show)
