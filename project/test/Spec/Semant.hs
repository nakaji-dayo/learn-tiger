module Spec.Semant where

import           AbSyn
import           Lexer            (AlexPosn (AlexPn))
import           Semant
import           Test.Tasty
import           Test.Tasty.HUnit
import           Ty

p = AlexPn 0 0 0

-- runTransRight
rtr m = runTrans m >>= either (assertFailure . show) pure
rtl m = runTrans m >>= either (const $ pure ()) (const $ assertFailure "unexpected success")

unit_transCall = do
  (_, ty) <- rtr $ trans (CallExp "print" [StringExp "foo" p] p)
  ty @?= Unit

unit_transOp = do
  (_, ty) <- rtr $ trans (OpExp (IntExp 0) Plus (IntExp 9) p)
  ty @?= Int

-- unit_transOpError = do
--   rtl $ trans (OpExp (StringExp "foo" p) Plus (StringExp "" p) p)
