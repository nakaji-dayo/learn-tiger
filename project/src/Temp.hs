{-# LANGUAGE FlexibleContexts #-}
module Temp where

import           Control.Monad.State (MonadState (get, put))
import           Temp.Type
import           Type

mkTemp :: MonadState (TransResult a) m => m Temp
mkTemp = do
  s <- get
  put (s {tempCounter = tempCounter s + 1})
  pure $ Temp $ tempCounter s

mkLabel :: MonadState (TransResult a) m => m Label
mkLabel = do
  s <- get
  put (s {labelCounter = labelCounter s + 1})
  pure $ Label $ Just $ "L" <> show (labelCounter s)

namedLabel :: String -> Label
namedLabel = undefined
