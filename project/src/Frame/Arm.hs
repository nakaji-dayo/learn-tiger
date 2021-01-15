{-# LANGUAGE TypeFamilies #-}
module Frame.Arm where

import           Assem.Type
import           Frame
import           Temp       (mkTemp)
import           Temp.Type
import qualified Tree       as T

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
  -- registers _ = specialregs <> argregs <> calleesaves <> callersaves
  -- やっぱ分けたい.Instrの内部まで知りたくない
  procEntryExit2 f b = b ++ [Oper "" (tempFP:tempLR:calleesaves) [] Nothing]

tempA0 = NamedTemp "a1"
tempFP = NamedTemp "fp"
tempSP = NamedTemp "fp"
tempLR = NamedTemp "fp"
specialregs = [tempFP, tempSP, tempLR]
argregs = NamedTemp . ('a':) . show <$> [1..4]
calleesaves = NamedTemp . ('v':) . show <$> [1..7]
callersaves = []
