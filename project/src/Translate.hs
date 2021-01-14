{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
module Translate where
import qualified AbSyn             as A
import           Capability.Reader (ask)
import           Capability.State  (HasState, get, put)
import           Frame             as F
import           Temp
import           Temp.Type
import           Translate.Type
import qualified Tree              as T
import           Type

--
newLevel :: Level -- ^ parent
         -> String -- ^ name
         -> [Bool] -- ^ formals
         -> Level
newLevel lv _ _ = lv + 1
 -- call Frame.newFrame


outermost :: Level
outermost = 0

formals :: Level ->[F.Access a]
formals = undefined

allocLocal :: (HasState "tempCounter" Int m, Frame f) => Level -> Bool -> m (F.Access f)
allocLocal _ _ = F.allocLocal undefined True

data Exp =
  Ex T.Exp
  | Nx T.Stm
  | Cx (Label -> Label -> T.Stm)

instance Show Exp where
  show (Cx mk) = show $ mk (Label (Just "stubT")) (Label (Just "stubF"))
  show (Nx x)  =  show x
  show (Ex x)  =  show x

unEx (Ex t)   = pure t
unEx (Nx stm) = pure $ T.ESeq stm (T.Const 0)
unEx (Cx mkstm) = do
  r <- mkTemp
  t <- mkLabel
  f <- mkLabel
  let stm = seqStm
            [ T.Move (T.Temp r) (T.Const 1)
            , mkstm t f
            , T.Label f
            , T.Move (T.Temp r) (T.Const 0)
            , T.Label t]
  pure $ T.ESeq stm (T.Temp r)

unNx :: TransC f m => Exp -> m T.Stm
unNx (Ex t)  = pure $ T.Exp t
unNx (Nx st) = pure st
unNx c       = T.Exp <$> unEx c

unCx (Ex e) t f = T.CJump T.NE e (T.Const 1) f t
unCx (Nx _) _ _ = error "impossible"
unCx (Cx c) t f = c t f

seq :: TransC f m => [Exp] -> m Exp
seq xs = do
  let h:t = reverse xs
  stms <- mapM unNx (reverse t)
  e <- unEx h
  pure $ Ex $ T.ESeq (seqStm stms) e

seqStm :: [T.Stm] -> T.Stm
seqStm = foldr T.Seq (T.Exp $ T.Const 0)

-- todo: 静的リンクの解決
simpleVar :: Frame a => Type.Access a
  -> Level -- ^ use from
  -> Translate.Exp
simpleVar (lv, a) _ = Ex $ F.exp a (T.Temp $ fp a)

letExp :: TransC f m => [Exp] -> Exp -> m Exp
letExp init b = do
  istms <- mapM unNx init
  be <- unEx b
  pure $ Ex $ T.ESeq (seqStm istms) be

assign :: TransC f m => Exp -> Exp -> m Exp
assign l r = Nx <$> (T.Move <$> unEx l <*> unEx r)

int :: Int -> Exp
int = Ex . T.Const

string :: TransC f m => String -> m Exp
string s = do
  lbl <- mkLabel
  fs <- get @"frags"
  let f = FString lbl s
  put @"frags" (f:fs)
  pure $ Ex $ T.Name lbl

binop A.Plus l r = Ex <$> (T.BinOp T.Plus <$> unEx l <*> unEx r)

call :: TransC f m => Level -> Level -> Label -> [Exp] -> m Exp
call erLv eeLv lbl as = do
  ase <- mapM unEx as
  pure $ Ex $ T.Call (T.Name lbl) ase
  -- todo: calc static link

ifExp :: TransC f m => Exp -> Exp -> Exp -> m Exp
ifExp c t f = do
  lt <- mkLabel
  lf <- mkLabel
  z <- mkLabel
  r <- mkTemp
  tex <- unEx t
  fex <- unEx f
  let s = seqStm
          [ unCx c lt lf
          , T.Label lt, T.Move (T.Temp r) tex
          , T.Jump (T.Name z) [z]
          , T.Label lf, T.Move (T.Temp r) fex
          , T.Jump (T.Name z) [z]
          , T.Label z
          ]
  pure $ Ex $ T.ESeq s (T.Temp r)
