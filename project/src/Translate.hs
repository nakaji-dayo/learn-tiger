{-# LANGUAGE AllowAmbiguousTypes #-}
module Translate where

import qualified AbSyn               as A
import           Control.Monad.State (MonadState (get, put))
import           Control.Monad.Trans (MonadTrans (lift))
import           Frame               as F
import           Temp
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
allocLocal _ _ = pure $ F.allocLocal undefined True

data Exp =
  Ex T.Exp
  | Nx T.Stm
  | Cx (Label -> Label -> T.Stm)

instance Show Exp where
  show (Cx mk) = show $ mk (Label (Just "stubT")) (Label (Just "stubF"))
  show (Nx x)  =  show x
  show (Ex x)  =  show x

unEx (Ex t)   = t
unEx (Nx stm) = T.ESeq stm (T.Const 0)
unEx (Cx mkstm) =
  let r = mkTemp
      t = mkLabel
      f = mkLabel
      stm = seqStm
        [ T.Move (T.Temp r) (T.Const 1)
        , mkstm t f
        , T.Label f
        , T.Move (T.Temp r) (T.Const 0)
        , T.Label t]
  in
    T.ESeq stm (T.Temp r)

unNx (Ex t)  = T.Exp t
unNx (Nx st) = st
unNx c       = T.Exp $ unEx c

unCx (Ex e) t f = T.Exp e
unCx (Nx _) _ _ = error "impossible"
unCx (Cx c) t f = c t f

seqStm :: [T.Stm] -> T.Stm
seqStm = foldr T.Seq (T.Exp $ T.Const 0)

-- todo: 静的リンクの解決
simpleVar :: Frame a => Type.Access a
  -> Level -- ^ use from
  -> Translate.Exp
simpleVar (lv, a) _ = Ex $ F.exp a (T.Temp $ fp a)

letExp :: [Exp] -> Exp -> Exp
letExp init b = Ex $ T.ESeq (seqStm $ fmap unNx init) (unEx b)

assign :: Exp -> Exp -> Exp
assign l r = Nx $ T.Move (unEx l) (unEx r)

int :: Int -> Exp
int = Ex . T.Const

string :: String -> TcM f Exp
string s = do
  r <- TcM $ lift get
  let lbl = mkLabel
  let f = FString lbl s
  TcM $ put $ r {frags = f:frags r}
  pure $ Ex $ T.Name lbl

binop A.Plus l r = Ex $ T.BinOp T.Plus (unEx l) (unEx r)
