{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Temp where

import           Capability.State
import           Temp.Type
import           Type

mkTemp :: HasState "tempCounter" Int m => m Temp
mkTemp = do
  s <- get @"tempCounter"
  modify @"tempCounter" ( + 1)
  pure $ Temp s

mkLabel :: HasState "labelCounter" Int m => m Label
mkLabel = do
  s <- get @"labelCounter"
  modify @"labelCounter" (+ 1)
  pure $ Label $ Just $ "L" <> show s

namedLabel :: String -> Label
namedLabel = undefined
