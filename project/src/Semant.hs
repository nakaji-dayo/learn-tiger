{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
module Semant where

import           AbSyn
import           Control.Monad        (forM, forM_, unless, void, when)
import           Control.Monad.Reader (MonadReader, ReaderT, asks, local,
                                       runReaderT)
import           Control.Monad.Trans  (MonadTrans (lift))
import           Data.HashMap.Strict  as H
import           Data.List            (sortBy, sortOn)
import           Data.Maybe           (catMaybes, fromMaybe, isNothing)
import           Debug.Trace          (trace, traceM, traceShowM)
import           GHC.Records
import           Lexer                (AlexPosn)
import           Ty
import           Util

type Imut = Bool
data VarEntry = VarEntry Imut Ty | FunEntry [Ty] Ty

data Env = Env
  { venv :: HashMap String VarEntry
  , tenv :: HashMap String Ty
  }

newtype TcM a = TcM {runTcM :: ReaderT Env (Either (Pos, String)) a}
  deriving ( Functor, Applicative, Monad
           , MonadReader Env)


lookupField pos t@(Record ts) sym =
  maybe (tyErr pos $ show sym <> " isn't defined in" <> show t) pure
  $ Prelude.lookup sym ts
lookupField pos t _ = tyErr pos $ show t <> " isn't record type"

lookupElem _ (Array t) =
  pure t
lookupElem pos t = tyErr pos $ show t <> " isn't array type"

transVar mut (SimpleVar s pos) = do
  if mut then getVarMut pos s else getVar' pos s
transVar mut (FieldVar var sym pos) = do
  t <- transVar mut var >>= actTy pos
  lookupField pos t sym
transVar mut (SubscriptVar var exp pos) = do
  t <- transVar mut var >>= actTy pos
  k <- transExp exp
  testTy pos Int k
  lookupElem pos t


transExp :: Exp -> TcM Ty.Ty
transExp (OpExp l op r pos) = do
  tl <- transExp l
  tr <-transExp r
  case (tl, tr) of
    (Int, Int) -> pure Int
    t | op `elem` [Eq, Neq] -> do
          r <- sEq pos tl tr
          if r then pure Int else tyErr pos ("not match " <> show (tl, op, tr))
      | otherwise -> tyErr pos $ "expect Int actual " <> show (op, tl, tr) <> show t
transExp IfExp{..} = do
  tt <- transExp test
  testTy pos Int tt
  tht <- transExp then'
  case else' of
    Just ele -> do
      elt <- transExp ele
      testTy pos tht elt
    _ ->
      testTy pos Unit tht

transExp WhileExp{..} = do
  tt <- transExp test
  testTy pos Int tt
  bt <- transExp body
  testTy pos Unit bt
transExp ForExp{..} = do
  lt <- transExp lo
  testTy pos Int lt
  ht <- transExp hi
  testTy pos Int ht
  defVar var (VarEntry True Int) $ do
    t <- transExp body
    testTy pos Unit t

transExp (IntExp _) = pure Int
transExp (StringExp _ _) = pure String
transExp NilExp = pure Nil
transExp (SeqExp []) = pure Unit -- おかしい
transExp (SeqExp [(x, _)]) = transExp x
transExp (SeqExp ((x, _):xs)) = transExp x >> transExp (SeqExp xs)
transExp (VarExp var) = transVar False var
transExp RecExp {..} = do
  t <- getTy' pos typ
  case t of
    Record fs
      | length fs == length fields -> pure ()
      | otherwise -> tyErr pos "todo: lookupField"
    _ -> tyErr pos "todo: lookupField"
  traceShowM ("-->>", typ, pos)

  forM_ fields $ \(sym', exp', pos') -> do
    t <- lookupField pos' t sym'
    t' <- transExp exp'
    traceShowM ("rec exp left name", sym')
    traceShowM (t, "==", t')
    r <- sEq pos t' t
    unless r $ tyErr pos ("(sEq) expected: " <> show t <> " actual: " <> show t' <> " , field:" <> sym')
    traceM "--end--"
    --   void $ testTy pos' t t'
  traceShowM ("<<--", typ, pos)
  pure t
transExp CallExp{..} = do
  (ats, rt) <- getFun pos func
  aats <- mapM transExp args
  when (length ats /= length aats) $ tyErr pos $ "called with " <> show (length aats) <> " args expect " <> show (length ats)
  forM_ (zip ats aats) $ uncurry (testTy pos)
  pure rt



-- 同ブロックの定義の後方参照は可能とする
--
-- todo: optimize, refactor
transExp LetExp{..} = do
  -- todo : 中間の型挟む
  let (vdecs, tdecs) = sepDecs decs
  ts' <- concat <$> mapM f' tdecs
  ts'' <- trace (show ts') $ resolveName ts'
  defTys (trace (show ts'') ts'') $ do
    -- vs' <- concat <$> mapM f vdecs
    -- defVars (fmap (\(x,y,_) -> (x,y)) vs') $ do
    tmpfold [] vdecs $ \p -> do
      sequence_ $ catMaybes p
      transExp body
  where
    tmpfold acc [] c = do
      c acc
    tmpfold acc (d:decs) c = do
      r <- f d
      defVars (fmap (\(x,y,_) -> (x,y)) r) $ tmpfold (fmap third3 r <> acc) decs c
    sepDecs :: [Dec] -> ([Dec], [Dec])
    sepDecs = Prelude.foldr f ([], [])
      where f d@(TyDecs _) (vs, ts) =  (vs, d:ts)
            f d (vs, ts)            =  (d:vs, ts)
    -- Maybe (TcM ()): promised type check
    f :: (Dec -> TcM [(Sym, VarEntry, Maybe (TcM ()))])
    f VarDec{..} = do
      t <- transExp init
      t' <- forM typ $ \(sym', pos') -> do
        t' <- getTy' pos' sym'
        when (t /= Nil) $ -- todo: 抽象化
          void $ testTy pos  t' t
        pure t'
      when (isNothing typ && t == Nil) $ tyErr pos "need type annotation"
      pure [(name, VarEntry False (fromMaybe t t'), Nothing)]
    f (FunDecs decs) =
      mapM g decs
    f' (TyDecs decs) =
      mapM h decs
    g FunDec{..} = do
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
            art <- defVars (mapSnd (VarEntry False) <$> ats) $
              transExp body
            void $ testTy pos rt art
      --
      pure (name, FunEntry (fmap snd ats) rt, Just p) -- todo: check duplicated
    h TyDec{..} = do
      t <- case ty of
        NameTy sym pos ->
          getTy sym >>=
           maybe (pure $ Name sym) pure
        RecordTy fs -> do
          ts <- forM fs $ \Field{..} -> do
            t <- getTy typ
              >>= maybe (pure $ Name typ) pure
            pure (name, t)
          pure $ Record ts
        ArrTy s pos ->
          maybe (pure $ Array (Name s)) (pure . Array) =<< getTy s
      pure (name, t)
    --相互再帰の解決（怪しい
    resolveName ts = do
      let f t = case t of
                    (Name sym) -> do
                      ty' <- getTy sym >>=
                        maybe (error  $ "undefined type: " <> sym) pure
                      pure $ t
                    (Array ty) -> do
                      ty' <- f ty
                      pure $ Array ty'
                    (Record fs) -> do
                      fs' <- forM fs $ \(sym, ty) -> do
                        ty' <- f ty
                        pure (sym, ty')
                      pure (Record fs')
                    _               -> pure t
      local (const $ Env empty (fromList ts)) $
        forM ts $ \(n, t) -> (n,) <$> f t
      -- pure ts
-- Nameの解決が定まらないので後回し（tcだけならシンプルにできるっぽいが、中間言語も気になる）

-- todo test16
-- todo: test17, 18
--   todo: test37, 38, 39
-- todo: test28, 29
-- let decsのdecsないで、type, var, typeのようにセパレートすると挙動変わるのはキモいのでやめようと思った: related test47

transExp ArrExp{..} = do
  st <- transExp size
  testTy pos Int st
  it <- transExp init
  getAcTy pos typ
    >>= \case
    at@(Array t) -> do
      testTy pos t it
      pure at
    t -> tyErr pos ("not array" <> typ <> ":" <> show t)

transExp AssignExp {..} = do
  t <- transVar True avar
  t' <- transExp exp
  case (t, t') of
    (_, Nil) -> pure ()
    _ ->
      void $ testTy pos t t'
  pure Unit

transExp e = error ("not implemented exp:" ++ show e)



defVar :: String
       -> VarEntry
       -> TcM a
       -> TcM a
defVar k v = local (\e -> e { venv = insert k v (venv e)})

defVars :: [(String, VarEntry)]
       -> TcM a
       -> TcM a
defVars xs = local (\e -> e { venv = foldl (\acc (k, v) -> insert k v acc) (venv e) xs})

defTys :: [(String, Ty)] -> TcM a -> TcM a
defTys xs = local (\e -> e { tenv = foldl (\acc (k, v) -> insert k v acc) (tenv e) xs})

getVar :: String -> TcM (Maybe VarEntry)
getVar k = asks (H.lookup k . venv)

getVar' pos k = getVar k >>= \case
  Just (VarEntry _ x) -> pure x
  Just _ -> (tyErr pos ("undefined variable '" ++ k ++ "'"))
  _ -> (tyErr pos ("undefined variable '" ++ k ++ "'"))

getVarMut pos k = getVar k >>= \case
  Just (VarEntry False x) -> pure x
  Just (VarEntry True x) -> (tyErr pos ("immutable var '" ++ k ++ "'"))
  Just _ -> (tyErr pos ("undefined variable '" ++ k ++ "'"))
  _ -> (tyErr pos ("undefined variable '" ++ k ++ "'"))

getFun pos k = getVar k >>= \case
  Just (FunEntry as rt) -> pure (as, rt)
  Just _ -> (tyErr pos ("undefined function '" ++ k ++ "'"))
  _ -> (tyErr pos ("undefined function '" ++ k ++ "'"))

getTy :: String -> TcM (Maybe Ty)
getTy k = asks (H.lookup k . tenv)

getTy' pos typ =
  getTy typ >>= maybe (tyErr pos ("undefined type: '" <> typ <> "'")) pure

-- tmp
getAcTy pos typ =
  getTy typ >>= \case
  Just (Name s) -> getAcTy pos s
  Just t -> pure t
  _ -> (tyErr pos ("undefined type: '" <> typ <> "'"))

actTy pos (Name t) = getTy' pos t
actTy _ t          = pure t
--

tyErr :: Pos -> String -> TcM a
tyErr pos msg = TcM $ lift $ Left (pos, msg)

testTy pos t' t = do
  r <- sEq pos t' t
  unless r $ tyErr pos ("doesn't match type '" <> show t <> "' with " <> show t' <> "'")
  pure t'


sEq :: AlexPosn -> Ty -> Ty -> TcM Bool
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
sEq pos (Name s) (Name s2) = pure $ s == s2
sEq pos (Name s) t = do
  at <- getTy' pos s
  sEq pos at t
sEq pos t (Name s) = do
  at <- getTy' pos s
  sEq pos t at
sEq _ x y = pure $ x == y



runTrans :: Exp -> Either (Pos, String) Ty
runTrans e = runTcM' (transExp e) initEnv

runTcM' x = runReaderT (runTcM x)

initEnv :: Env
initEnv =
  let venv = fromList
        [ ("print", FunEntry [String] Unit)
        , ("flush", FunEntry [] Unit)
        , ("getchar", FunEntry [] String)
        , ("ord", FunEntry [String] Int)
        , ("chr", FunEntry [Int] String)
        , ("size", FunEntry [String] Int)
        , ("substring", FunEntry [String, Int, Int] String)
        , ("concat", FunEntry [String, String] String)
        , ("not", FunEntry [Int] Int)
        , ("exit", FunEntry [Int] Unit)
        ]
      tenv = fromList
        [ ("int", Int)
        , ("string", String)
        ]
  in Env venv tenv
