{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Type where
import           AbSyn
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.HashMap.Lazy
import           Frame                as F
import           Temp                 (Label (Label))
import qualified Tree                 as T
import           Ty

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


data Frag a = FProc T.Stm a
            | FString Label String

data TransResult a = TransResult
  { frags :: [Frag a]
  }

newtype TcM f a = TcM {runTcM :: ReaderT (Env f) (StateT (TransResult f) (Either (Pos, String))) a}
  deriving ( Functor, Applicative, Monad
           , MonadReader (Env f))
