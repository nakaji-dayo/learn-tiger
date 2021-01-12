{-# LANGUAGE TypeFamilies #-}
module Frame.Arm where

import           Frame
import           Temp      (mkTemp)
import           Temp.Type
import qualified Tree      as T

data ArmFrame = ArmFrame
  { fname    :: Label
  , fformals :: [ArmAccess]
  }

data ArmAccess =
  InFrame Int
  | InReg Temp

instance Frame ArmFrame where
  type Access ArmFrame = ArmAccess
  newFrame l fs =
    ArmFrame l (zipWith (curry (InReg . Temp . fst)) [0..] fs)
  name = fname
  formals = fformals
  allocLocal f _ = do
    InReg <$> mkTemp
  fp _ = Temp 11 -- こうではない。既彩色として扱われればよいだけ
  rv _ = Temp 0
  wordSize _ = 4
  exp (InReg t) _    = T.Temp t
  exp (InFrame k) fp = T.Mem $ T.BinOp T.Plus fp (T.Const k)
