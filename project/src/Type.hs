{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Type where
import           AbSyn
import           Data.HashMap.Lazy
import           Frame                as F
import           Temp.Type
import           Translate.Type
import qualified Tree                 as T
import           Ty
import Capability.Source
import Capability.Sink
import Capability.State as C
import Data.IORef
import GHC.Generics (Generic)
import Control.Monad.Reader (ReaderT(ReaderT))
import Capability.Reader

-- Translate?
type Level = Int
type Access a = (Level, F.Access a)
--

type Imut = Bool
data VarEntry a =
  VarEntry (Type.Access a) Imut Ty
  | FunEntry Level Label [Ty] Ty

data Env a = Env
  { venv  :: HashMap String (VarEntry a)
  , tenv  :: HashMap String Ty
  , level :: Level
  }

data Ctx f = Ctx
  { frags        :: IORef [Frag f]
  , tempCounter  :: IORef Int
  , labelCounter :: IORef Int
  , env :: Env f
  }
  deriving (Generic)

newtype SemM f a = SemM (ReaderT (Ctx f) IO a)
  deriving (Functor, Applicative, Monad)
  deriving (HasSource "tempCounter" Int, HasSink "tempCounter" Int, HasState "tempCounter" Int) via ReaderIORef (C.Field "tempCounter" () (MonadReader (ReaderT (Ctx f) IO)))
  deriving (HasSource "labelCounter" Int, HasSink "labelCounter" Int, HasState "labelCounter" Int) via ReaderIORef (C.Field "labelCounter" () (MonadReader (ReaderT (Ctx f) IO)))
  deriving (HasSource "frags" [Frag f], HasSink "frags" [Frag f], HasState "frags" [Frag f]) via ReaderIORef (C.Field "frags" () (MonadReader (ReaderT (Ctx f) IO)))
  deriving (HasSource "env" (Env f), HasReader "env" (Env f)) via
    (C.Field "env" () (MonadReader (ReaderT (Ctx f) IO)))


type TransC a m = ( HasState "tempCounter" Int m
                  , HasState "labelCounter" Int m
                  , HasState "frags" [Frag a] m
                  , HasReader "env" (Env a) m)
