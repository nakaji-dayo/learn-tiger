{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
module Semant where

import           AbSyn
import           Control.Monad        (forM, forM_, unless, void, when)
import           Control.Monad.Reader (MonadReader, ReaderT, asks, local,
                                       runReaderT)
import           Control.Monad.State  (evalState, evalStateT, runState)
import           Control.Monad.Trans  (MonadTrans (lift))
import           Data.HashMap.Strict  as H
import           Data.List            (sortOn)
import           Data.Maybe           (catMaybes, fromMaybe, isNothing)
import           Debug.Trace          (trace, traceM, traceShowM)
import           Frame                (Frame)
import           Frame.Arm            (ArmFrame (ArmFrame))
import           GHC.Records
import           GHC.Stack            (HasCallStack)
import           Lexer                (AlexPosn)
import           Temp                 (Label (Label), mkLabel)
import           Translate            (newLevel, outermost)
import qualified Translate            as T
import           Ty
import           Type
import           Util

lookupField pos t@(Record ts) sym =
  maybe (tyErr pos $ show sym <> " isn't defined in" <> show t) pure
  $ Prelude.lookup sym ts
lookupField pos t _ = tyErr pos $ show t <> " isn't record type"

lookupElem _ (Array t) =
  pure t
lookupElem pos t = tyErr pos $ show t <> " isn't array type"


transVar :: Frame f => Bool -> Var -> TcM f (T.Exp, Ty)
transVar mut (SimpleVar s pos) = do
  (a, ty) <- if mut then getVarMut pos s else getVar' pos s
  let e = T.simpleVar a (error "transVar level")
  pure (e, ty)
transVar mut (FieldVar var sym pos) = do
  t <- transVar mut var >>= actTy pos . snd
  ty <- lookupField pos t sym
  pure (undefined, ty)
transVar mut (SubscriptVar var exp pos) = do
  t <- transVar mut var >>= actTy pos . snd
  (kexp, k) <- transExp exp
  testTy pos Int k
  t' <- lookupElem pos t
  pure (undefined, t')

errTExp :: HasCallStack => T.Exp
errTExp = error "not implemented T.Exp"

transExp :: Frame f => Exp -> TcM f (T.Exp, Ty.Ty)
transExp (OpExp l op r pos) = do
  (lexp, tl) <- transExp l
  (rexp, tr) <- transExp r
  case (tl, tr) of
    (Int, Int) -> do
       pure (T.binop op lexp rexp, Int)
    t | op `elem` [Eq, Neq] -> do
          r <- sEq pos tl tr
          if r then pure (errTExp, Int) else tyErr pos ("not match " <> show (tl, op, tr))
      | otherwise -> tyErr pos $ "expect Int actual " <> show (op, tl, tr) <> show t
transExp IfExp{..} = do
  (texp, tt) <- transExp test
  testTy pos Int tt
  (thexp, tht) <- transExp then'
  case else' of
    Just ele -> do
      (elexp, elt) <- transExp ele
      testTy pos tht elt
      pure (errTExp, tht)
    _ -> do
      testTy pos Unit tht
      pure (errTExp, Unit)

transExp WhileExp{..} = do
  (texp, tt) <- transExp test
  testTy pos Int tt
  (bexp, bt) <- transExp body
  (errTExp, ) <$> testTy pos Unit bt
transExp ForExp{..} = do
  (lexp, lt) <- transExp lo
  testTy pos Int lt
  (hexp, ht) <- transExp hi
  testTy pos Int ht
  defVar var (VarEntry (error "defVar T.Exp") True Int) $ do
    (exp, t) <- transExp body
    (errTExp, ) <$> testTy pos Unit t


transExp (IntExp x) = pure (T.int x, Int)
transExp (StringExp x _) = do
  (, String) <$> T.string x
transExp NilExp = pure (T.int 0, Nil) -- 本当?
transExp (SeqExp []) = pure (errTExp, Unit) -- 怪しい
transExp (SeqExp [(x, _)]) = transExp x
transExp (SeqExp ((x, _):xs)) = transExp x >> transExp (SeqExp xs)
transExp (VarExp var) = transVar False var
transExp RecExp {..} = do
  t <- getTy' pos typ
  case t of
    Record fs
      | length fs == length fields -> pure ()
      | otherwise -> tyErr pos "todo: lookupField"
    _ -> tyErr pos "todo: lookupField2"
  traceShowM ("-->>", typ, pos)

  forM_ fields $ \(sym', exp', pos') -> do
    t <- lookupField pos' t sym'
    (t'exp, t') <- transExp exp'
    traceShowM ("rec exp left name", sym')
    traceShowM (t, "==", t')
    r <- sEq pos t' t
    unless r $ tyErr pos ("(sEq) expected: " <> show t <> " actual: " <> show t' <> " , field:" <> sym')
    traceM "--end--"
    --   void $ testTy pos' t t'
  traceShowM ("<<--", typ, pos)
  pure (errTExp, t)
transExp CallExp{..} = do
  (ats, rt) <- getFun pos func
  aats <- mapM transExp args
  when (length ats /= length aats) $ tyErr pos $ "called with " <> show (length aats) <> " args expect " <> show (length ats)
  forM_ (zip ats (snd <$> aats)) $ uncurry (testTy pos)
  pure (errTExp, rt)

-- 同ブロックの定義の後方参照は可能とする
--
-- todo: optimize, refactor
transExp LetExp{..} = do
  -- todo : 中間の型挟む
  let (vdecs, tdecs) = sepDecs decs
  ts' <- concat <$> mapM transTDecs tdecs
  ts'' <- resolveName ts'
  (bexp, bty, iexps) <- defTys ts'' $ do
    foldVDecs ([], []) vdecs $ \(tcs, iexps) -> do
      sequence_ $ catMaybes tcs
      (exp, ty) <- transExp body
      pure (exp, ty, iexps)
  let letexp = T.letExp iexps bexp
  pure (letexp, bty)
  where
    foldVDecs ::  Frame f => ([Maybe (TcM f ())], [T.Exp])
                  -> [Dec] -> (([Maybe (TcM f ())], [T.Exp]) -> TcM f b) -> TcM f b
    foldVDecs acc [] c = do
      c acc
    foldVDecs (lazyc, inits) (d:decs) c = do
      (r, iexp) <- transVDec d
      let inits' = maybe inits (:inits) iexp
      defVars (fmap (\(x,y,_) -> (x,y)) r) $
        foldVDecs (fmap third3 r <> lazyc, inits') decs c
    -- TODO: decの型整理
    sepDecs :: [Dec] -> ([Dec], [Dec])
    sepDecs = Prelude.foldr f ([], [])
      where f d@(TyDecs _) (vs, ts) =  (vs, d:ts)
            f d (vs, ts)            =  (d:vs, ts)
    -- Maybe (TcM f ()): promised type check
    transVDec :: forall f. Frame f => Dec
              -> TcM f ([(Sym, VarEntry f, Maybe (TcM f ()))], Maybe T.Exp)
    transVDec VarDec{..} = do
      lv <- getLevel
      ac <- T.allocLocal lv True
      (exp, t) <- transExp init
      let var = T.simpleVar (lv, ac) lv
          assign = T.assign var exp
      t' <- forM typ $ \(sym', pos') -> do
        t' <- getTy' pos' sym'
        when (t /= Nil) $ -- todo: 抽象化
          void $ testTy pos  t' t
        pure t'
      when (isNothing typ && t == Nil) $ tyErr pos "need type annotation"
      pure ([(name, VarEntry (lv, ac) False (fromMaybe t t'), Nothing)], Just assign)
    transVDec (FunDecs decs) =
      (, Nothing) <$> mapM transFunDec decs
    transTDecs :: Dec -> TcM f [(Sym, Ty)]
    transTDecs (TyDecs decs) =
      mapM transTDec decs
    transFunDec :: Frame f => FunDec -> TcM f (Sym, VarEntry f, Maybe (TcM f ()))
    transFunDec FunDec{..} = do
      ats <- forM params $ \p -> do
        let s = getField @"typ" p
            n = getField @"name" p
        t <- getTy' (getField @"pos" p) s
        pure (n, t)
      rt <- case result of
        Just (rts, pos) -> getTy' pos rts
        _               -> pure Unit
      -- func body
      let p = do
            (bexp, art) <- defVars (mapSnd (VarEntry (error "T.Exp funargs") False) <$> ats) $
              -- todo: use escape
              newLevel' "todo:label" (replicate (length params) False) $
              transExp body
            void $ testTy pos rt art
      --
      lv <- getLevel
      traceM $ "debug: level:" <> show lv
      let lbl = mkLabel
      pure (name, FunEntry lv lbl (fmap snd ats) rt, Just p) -- todo: check duplicated
    transTDec :: TyDec -> TcM f (Sym, Ty)
    transTDec TyDec{..} = do
      t <- case ty of
        NameTy sym _ ->
          getTy sym >>=
           maybe (pure $ Name sym) pure
        RecordTy fs -> do
          ts <- forM fs $ \Field{..} -> do
            t <- getTy typ
              >>= maybe (pure $ Name typ) pure
            pure (name, t)
          pure $ Record ts
        ArrTy s _ ->
          maybe (pure $ Array (Name s)) (pure . Array) =<< getTy s
      pure (name, t)
    --相互再帰の解決（怪しい
    resolveName :: [(String, Ty)] -> TcM f [(String, Ty)]
    resolveName ts = do
      let f t = case t of
                    (Name sym) -> do
                      getTy sym >>=
                        maybe (error  $ "undefined type: " <> sym) pure
                      pure t
                    (Array ty) -> do
                      ty' <- f ty
                      pure $ Array ty'
                    (Record fs) -> do
                      fs' <- forM fs $ \(sym, ty) -> do
                        ty' <- f ty
                        pure (sym, ty')
                      pure (Record fs')
                    _               -> pure t
      lv <- getLevel
      local (const $ Env empty (fromList ts) lv) $
        forM ts $ \(n, t) -> (n,) <$> f t
      -- pure ts
-- Nameの解決が定まらないので後回し（tcだけならシンプルにできるっぽいが、中間言語も気になる）

-- todo test16
-- todo: test17, 18
--   todo: test37, 38, 39
-- todo: test28, 29
-- let decsのdecsないで、type, var, typeのようにセパレートすると挙動変わるのはキモいのでやめようと思った: related test47

transExp ArrExp{..} = do
  (sexp, st) <- transExp size
  testTy pos Int st
  (iexp, it) <- transExp init
  getAcTy pos typ
    >>= \case
    at@(Array t) -> do
      testTy pos t it
      pure (errTExp, at)
    t -> tyErr pos ("not array" <> typ <> ":" <> show t)

transExp AssignExp {..} = do
  (_, t) <- transVar True avar
  (t'exp, t') <- transExp exp
  case (t, t') of
    (_, Nil) -> pure ()
    _ ->
      void $ testTy pos t t'
  pure (errTExp, Unit)

transExp e = error ("not implemented exp:" ++ show e)



defVar :: String
       -> VarEntry f
       -> TcM f a
       -> TcM f a
defVar k v = local (\e -> e { venv = insert k v (venv e)})

defVars :: [(String, VarEntry f)]
       -> TcM f a
       -> TcM f a
defVars xs = local (\e -> e { venv = foldl (\acc (k, v) -> insert k v acc) (venv e) xs})

defTys :: [(String, Ty)] -> TcM f a -> TcM f a
defTys xs = local (\e -> e { tenv = foldl (\acc (k, v) -> insert k v acc) (tenv e) xs})

getVar :: String -> TcM f (Maybe (VarEntry f))
getVar k = asks (H.lookup k . venv)

getVar' pos k = getVar k >>= \case
  Just (VarEntry a _ x) -> pure (a, x)
  Just _ -> (tyErr pos ("undefined variable '" ++ k ++ "'"))
  _ -> (tyErr pos ("undefined variable '" ++ k ++ "'"))

getVarMut :: Pos -> String -> TcM f (Access f, Ty)
getVarMut pos k = getVar k >>= \case
  Just (VarEntry a False x) -> pure (a, x)
  Just (VarEntry _ True x) -> (tyErr pos ("immutable var '" <> k <> "'" <> ":" <> show x))
  Just _ -> (tyErr pos ("undefined variable '" ++ k ++ "'"))
  _ -> (tyErr pos ("undefined variable '" ++ k ++ "'"))

getFun pos k = getVar k >>= \case
  Just (FunEntry _ _ as rt) -> pure (as, rt)
  Just _ -> (tyErr pos ("undefined function '" ++ k ++ "'"))
  _ -> (tyErr pos ("undefined function '" ++ k ++ "'"))

getTy :: String -> TcM f (Maybe Ty)
getTy k = asks (H.lookup k . tenv)

getTy' pos typ =
  getTy typ >>= maybe (tyErr pos ("undefined type: '" <> typ <> "'")) pure

getLevel :: TcM f Level
getLevel = asks level

newLevel' :: String -> [Bool] -> TcM f b -> TcM f b
newLevel' name formals c = do
  lv <- getLevel
  let lv' = newLevel lv name formals
  local (\e -> e { level = lv' }) c

-- tmp
getAcTy pos typ =
  getTy typ >>= \case
  Just (Name s) -> getAcTy pos s
  Just t -> pure t
  _ -> (tyErr pos ("undefined type: '" <> typ <> "'"))

actTy pos (Name t) = getTy' pos t
actTy _ t          = pure t
--

tyErr :: Pos -> String -> TcM f a
tyErr pos msg = TcM $ lift . lift $ Left (pos, msg)

testTy pos t' t = do
  r <- sEq pos t' t
  unless r $ tyErr pos ("doesn't match type '" <> show t <> "' with " <> show t' <> "'")
  pure t'


sEq :: AlexPosn -> Ty -> Ty -> TcM f Bool
sEq pos (Record xs) (Record ys)
  | length xs == length ys = do
      traceShowM (xs, ys)
      let zfs = zip (sortOn fst xs) (sortOn fst ys)
          f ((s1, t1), (s2, t2))
            | s1 == s2 = sEq pos t1 t2
            | otherwise = pure False
      and <$> mapM f zfs
  | otherwise = pure False
  -- fmap and $ mapM (\((_, x), (_, y)) -> sEq pos x y) $ zip xs ys
sEq p (Array x') (Array y') = sEq p x' y'
sEq _ Nil _               = pure True
sEq _ _ Nil               = pure True
sEq _ (Name s) (Name s2) = pure $ s == s2
sEq pos (Name s) t = do
  at <- getTy' pos s
  sEq pos at t
sEq pos t (Name s) = do
  at <- getTy' pos s
  sEq pos t at
sEq _ x y = pure $ x == y



runTrans :: Exp -> Either (Pos, String) (T.Exp, Ty)
runTrans e = runTcM' (transExp e) initEnv initTrans

runTcM' :: TcM ArmFrame  a -> Env ArmFrame -> TransResult ArmFrame  -> Either (Pos, String) a
runTcM' x e e' = evalStateT (runReaderT (runTcM x) e) e'

initEnv :: Frame f => Env f
initEnv =
  let venv = fromList
        [ ("print", FunEntry outermost (Label (Just "print")) [String] Unit)
        , ("flush", FunEntry outermost (Label (Just "flush")) [] Unit)
        , ("getchar", FunEntry outermost (Label (Just "getchar")) [] String)
        , ("ord", FunEntry outermost (Label (Just "ord")) [String] Int)
        , ("chr", FunEntry outermost (Label (Just "chr")) [Int] String)
        , ("size", FunEntry outermost (Label (Just "size")) [String] Int)
        , ("substring", FunEntry outermost (Label (Just "substring")) [String, Int, Int] String)
        , ("concat", FunEntry outermost (Label (Just "concat")) [String, String] String)
        , ("not", FunEntry outermost (Label (Just "not")) [Int] Int)
        , ("exit", FunEntry outermost (Label (Just "exit")) [Int] Unit)
        ]
      tenv = fromList
        [ ("int", Int)
        , ("string", String)
        ]
      level = outermost
  in Env {..}

initTrans = TransResult []
