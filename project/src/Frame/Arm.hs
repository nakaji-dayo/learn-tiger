{-# LANGUAGE TypeFamilies #-}
module Frame.Arm where

import           Assem.Type
import           Data.List       (intercalate)
import qualified Data.Map.Strict as M
import           Frame
import           Temp            (mkTemp)
import           Temp.Type
import           Translate.Type  (Frag (FString))
import qualified Tree            as T

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
  procEntryExit2 f b = b ++ [Oper mempty (tempFP:tempLR:calleesavetmps) [] Nothing]


  registers _ = calleesaves <> ["fp","sp", "lr"]
  tempMap _ = M.fromList $ fmap toPair $ [tempA0] <> specialregs

tempA0 = NamedTemp "a1"
tempFP = NamedTemp "fp"
tempSP = NamedTemp "sp"
tempLR = NamedTemp "lr"
specialregs = [tempFP, tempSP, tempLR]
argregs = ('a':) . show <$> [1..4]
argtmps = NamedTemp  <$> argregs
calleesaves = ('v':) . show <$> [1..7]
calleesavetmps = NamedTemp <$> calleesaves
callersaves = []

toPair t@(NamedTemp n) = (t,n)



formatString :: [Frag ArmFrame] -> String
formatString = intercalate "\n\n" . fmap f
  where f (FString (Label (Just l)) s) =
          let lc = "." <> l <> "C"
          in intercalate "\n"
             [ lc <> ":"
             , "  .word " <> show (length s) -- todo: bug: escape文字のlengthが2になる
             , "  .ascii \"" <> s  <> "\""
             , l <> ":"
             ,"  .word " <> lc
             , "\n"
             ]


buildMain x = intercalate "\n"
              [ ".global tigermain"
              , "tigermain:"
              , "  push {fp, lr}"]
              <> "\n" <> x <> "\n"
              <> intercalate "\n"
              [ "  pop {fp, lr}"
              , "  bx lr"]
