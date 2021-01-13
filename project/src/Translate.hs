{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
module Translate where

import qualified AbSyn               as A
import           Control.Monad.State (MonadState (get, put))
import           Control.Monad.Trans (MonadTrans (lift))
import           Frame               as F
import           Temp
import           Temp.Type
import           Translate.Type
import qualified Tree                as T
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

allocLocal :: Frame f => Level -> Bool -> TcM f (F.Access f)
allocLocal _ _ = TcM $ F.allocLocal undefined True

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
  r <- TcM mkTemp
  t <- TcM mkLabel
  f <- TcM mkLabel
  let stm = seqStm
            [ T.Move (T.Temp r) (T.Const 1)
            , mkstm t f
            , T.Label f
            , T.Move (T.Temp r) (T.Const 0)
            , T.Label t]
  pure $ T.ESeq stm (T.Temp r)

unNx :: Exp -> TcM a T.Stm
unNx (Ex t)  = pure $ T.Exp t
unNx (Nx st) = pure st
unNx c       = T.Exp <$> unEx c

unCx (Ex e) t f = T.CJump T.NE e (T.Const 1) f t
unCx (Nx _) _ _ = error "impossible"
unCx (Cx c) t f = c t f

seq :: [Exp] -> TcM f Exp
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

letExp :: [Exp] -> Exp -> TcM f Exp
letExp init b = do
  istms <- mapM unNx init
  be <- unEx b
  pure $ Ex $ T.ESeq (seqStm istms) be

assign :: Exp -> Exp -> TcM f Exp
assign l r = Nx <$> (T.Move <$> unEx l <*> unEx r)

int :: Int -> Exp
int = Ex . T.Const

string :: String -> TcM f Exp
string s = do
  lbl <- TcM mkLabel
  r <- TcM get
  let f = FString lbl s
  TcM $ put $ r {frags = f:frags r}
  pure $ Ex $ T.Name lbl

binop A.Plus l r = Ex <$> (T.BinOp T.Plus <$> unEx l <*> unEx r)

call :: Level -> Level -> Label -> [Exp] -> TcM f Exp
call erLv eeLv lbl as = do
  ase <- mapM unEx as
  pure $ Ex $ T.Call (T.Name lbl) ase
  -- todo: calc static link

ifExp :: Exp -> Exp -> Exp -> TcM f Exp
ifExp c t f = do
  lt <- TcM mkLabel
  lf <- TcM mkLabel
  z <- TcM mkLabel
  r <- TcM mkTemp
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
