{-# LANGUAGE DuplicateRecordFields #-}
module AbSyn where

import           Lexer (AlexPosn)

type Pos = AlexPosn
type Sym = String

data Var =
  SimpleVar Sym Pos
  | FieldVar Var Sym Pos
  | SubscriptVar Var Exp Pos
  deriving (Show)

-- Posくくり出したいが、とりあえず図の通り

data Exp =
  VarExp Var
  | NilExp
  | IntExp Int
  | StringExp String Pos
  | CallExp { func :: Sym, args :: [Exp], pos:: Pos}
  | OpExp {left :: Exp, oper :: Oper, right :: Exp, pos :: Pos}
  | RecExp {fields :: [(Sym, Exp, Pos)], typ :: Sym, pos :: Pos}
  | SeqExp [(Exp, Pos)]
  | AssignExp {avar :: Var, exp :: Exp, pos :: Pos}
  | IfExp {test :: Exp, then' :: Exp, else' :: Maybe Exp, pos :: Pos}
  | WhileExp {test :: Exp, body :: Exp, pos :: Pos}
  | ForExp {var :: Sym, lo :: Exp, hi :: Exp, body :: Exp, pos :: Pos} -- escapeまだ理解できていない
  | BreakExp Pos
  | LetExp {descs :: [Dec], body :: Exp, pos :: Pos}
  | ArrExp {typ :: Sym, size :: Exp, init :: Exp, pos :: Pos}
  deriving (Show)

data Dec =
  FunDecs [FunDec]
  | VarDec {name :: Sym, typ :: Maybe (Sym, Pos), init :: Exp, pos :: Pos} -- escape
  | TypeDecs [TyDec]
  deriving (Show)

data TyDec = TyDec {name :: Sym, ty :: Ty, pos :: Pos}
  deriving (Show)

data Ty =
  NameTy Sym Pos
  | RecordTy [Field]
  | ArrTy Sym Pos
  deriving (Show)

data Field = Field
  { name :: Sym
  -- escape
  , typ  :: Sym
  , pos  :: Pos
  }
  deriving (Show)

data FunDec = FunDec
  { name   :: Sym
  , params :: [Field]
  , result :: Maybe (Sym, Pos)
  , body   :: Exp
  , pos    :: Pos
  }
  deriving (Show)

data Oper = Plus | Minus | Times | Divide | Eq | Neq | Lt | Lte | Gt | Gte
  deriving (Show)
