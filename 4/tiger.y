{
module Main where

import Prelude hiding (GT, LT)
import qualified AbSyn as A
import Lexer
import Text.Pretty.Simple (pPrint)
}

%name calc
%tokentype { TigToken }
%error { parseError }

%monad { Alex }
%lexer { lexwrap } { (_, EOF) }

%token
    if          { (_, $$@If) }
    '-'         { (_, $$@Negation) }
    '('         { (_, $$@LParen) }
    ')'         { (_, $$@RParen) }
    '{'         { (_, $$@LBrace) }
    '}'         { (_, $$@RBrace) }
    '['         { (_, $$@LBracket) }
    ']'         { (_, $$@RBracket) }
    ','         { (_, $$@Comma) }
    ':'         { (_, $$@Colon) }
    ';'         { (_, $$@Semicolon) }
    type        { (_, $$@Type) }
    then        { (_, $$@Then) }
    else        { (_, $$@Else) }
    var         { (_, $$@Var)      }
    function    { (_, $$@Function) }
    let         { (_, $$@Let)      }
    in          { (_, $$@In)       }
    end         { (_, $$@End)      }
    ':='        { (_, $$@Bind)     }
    array       { (_, $$@Array)    }
    of          { (_, $$@Of)       }
    while       { (_, $$@While)    }
    for         { (_, $$@For)      }
    to          { (_, $$@To)       }
    break       { (_, $$@Break)    }
    do          { (_, $$@Do)       }
    nil         { (_, $$@Nil)      }
    '+'         { (_, $$@Plus)  }
    '*'         { (_, $$@Times)  }
    '/'         { (_, $$@Div)   }
    '='         { (_, $$@Eq)    }
    '<>'        { (_, $$@NotEq) }
    '<'         { (_, $$@LT)    }
    '<='        { (_, $$@LTE)   }
    '>'         { (_, $$@GT)    }
    '>='        { (_, $$@GTE)   }
    '&'         { (_, $$@And)   }
    '|'         { (_, $$@Or)    }
    '.'         { (_, $$@Dot)    }
    id          { (_, Id $$) }
    num         { (_, Num $$) }
--    real        { (_, Real $$) }
    str         { (_, StringL $$) }

%left atomexp -- 怪しい

%left '=' '<>' '<' '<=' '>' '>='
%left '&' '|'
%left '+' '-'
%left '*' '/'
%right of
%right ':='
%left negexp

%nonassoc ifexp -- if then, if then elseに文法上の競合がある。if thenの還元よりelseのシフトを優先した(多くの言語に習いelseは近いthenと繋がる)
%nonassoc else

%%

-- monadic parserが有用な気がするが、後で直す
-- (?, Id $$)みたいなケースでキモい
-- 構造直す
--
-- pos取りやすくなったが、lexがtoken吸った後のposになってしまっている
-- happyのexampleにもこの問題の記述あるっぽい。あとで考える

-- TODO: 例では要素1のSeqは作られないっぽい
Exp   : let Decs in Seq end  {% pos $ A.LetExp (reduceDecs $2) $4 }
      | Exp1 %prec atomexp { $1 }

Decs  : Dec Decs   { $1:$2 }
      | Dec        { [$1] }

Dec   : TyDec                   { $1 }
      | VarDec                  { $1 }
      | FunDec                  { $1 }

TyDec : type id '=' Ty          {% fmap (\x -> A.TypeDecs [x]) $ pos (A.TyDec $2 $4) }

-- :: (String, AlexPosn)
TyId  : id                      {% pos (\p -> ($1, p)) }

Ty    : id                    {% pos $ A.NameTy $1 }
      | array of id           {% pos $ A.ArrTy $3 }
      | '{' TyFs '}' { A.RecordTy $2 }

TyFs  : id ':' id ',' TyFs        {% pos $ \p -> (A.Field $1 $3 p):$5 }
      | id ':' id                 {% pos $ \p -> [A.Field $1 $3 p] }
      |                           { [] }

VarDec: var id ':=' Exp           {% pos $ A.VarDec $2 Nothing $4 }
      | var id ':' TyId ':=' Exp    {% pos $ A.VarDec $2 (Just $4) $6 }

OptTyAnn: ':' TyId { Just $2 }
        |          { Nothing }

FunDec: --function id '(' TyFs ')' '=' Exp1          { (error "error:14") }
        function id '(' TyFs ')' OptTyAnn '=' Exp1
          {% pos $ \p -> A.FunDecs [A.FunDec $2 $4 $6 $8 p] }
-- fundecsどこでまとめる？後処理みたいなフェーズがあれば適切なのでは?

Seq   : Exp {% pos $ \p -> A.SeqExp [($1, p)]  }
      | Exp ';' Seq {% pos $ (\(A.SeqExp xs) p -> A.SeqExp (($1,p):xs)) $3 }

LVal : id {% pos $ A.SimpleVar $1 }
     | LVal1 { $1 }

LVal1 : id '[' Exp1 ']' {% pos $ \p -> A.SubscriptVar (A.SimpleVar $1 p) $3 p}
      | LVal1 '[' Exp1 ']' {% pos $ A.SubscriptVar $1 $3}
      | id '.' id {% pos $ \p -> A.FieldVar (A.SimpleVar $1 p) $3 p}
      | LVal1 '.' id {% pos $ A.FieldVar $1 $3}

-- LVal  : id ValAcc {% pos $ A.SimpleVar $1 }

-- ValAcc: '.' id ValAcc { (error "error:18") }
--       | '[' Exp1 ']' ValAcc { (error "error:19") }
--       |   { A.NilExp }

Exp1  : Exp1 '+' Exp1           {% pos $ A.OpExp $1 A.Plus $3 }
      | Exp1 '-' Exp1           {% pos $ A.OpExp $1 A.Minus $3}
      | Exp1 '*' Exp1           {% pos $ A.OpExp $1 A.Times $3}
      | Exp1 '/' Exp1           {% pos $ A.OpExp $1 A.Divide $3}
      | Exp1 '&' Exp1           {% pos $ A.IfExp $1 $3 (Just $1) }
      | Exp1 '|' Exp1           {% pos $ A.IfExp $1 $1 (Just $3) }
      | Exp1 '=' Exp1 {% pos $ A.OpExp $1 A.Eq $3}
      | Exp1 '<>' Exp1 {% pos $ A.OpExp $1 A.Neq $3}
      | Exp1 '>' Exp1 {% pos $ A.OpExp $1 A.Gt $3}
      | Exp1 '<' Exp1 {% pos $ A.OpExp $1 A.Lt $3}
      | Exp1 '>=' Exp1 {% pos $ A.OpExp $1 A.Gte $3}
      | Exp1 '<=' Exp1 {% pos $ A.OpExp $1 A.Lte $3}
      | LVal ':=' Exp1 {% pos $ A.AssignExp $1 $3 }
      | Atom                    { $1 }
      | id '(' Vals ')' {% pos $ A.CallExp $1 $3 }
      | '(' Seq ')'             { $2 }
      | '(' ')' { A.SeqExp [] }
      | while Exp1 do Exp {% pos $ A.WhileExp $2 $4 }
      | for id ':=' Exp1 to Exp1 do Exp {% pos $ A.ForExp $2 $4 $6 $8 }
      | if Exp1 then Exp %prec ifexp {% pos $ A.IfExp $2 $4 Nothing }
      | if Exp1 then Exp else Exp {% pos $ A.IfExp $2 $4 (Just $6)}
      | break {% pos A.BreakExp }

Vals  : Exp1 { [$1] }
      | Exp1 ',' Vals { $1: $3 }
      | { [] }

Atom
      : num                     { A.IntExp $1 }
      -- | real                     { (error "error:43") }
      | str {% pos $ A.StringExp $1 }
      -- | id {% Alex $ \s ->  Right (s, A.VarExp (A.SimpleVar ($1) (alex_pos s))) }
      | nil { A.NilExp }
      | id '{' RecFs '}' {% pos $ A.RecExp $3 $1 }
      -- | id '.' id { (error "error:48") }
      | id '[' Exp1 ']' Arr1 {% pos $ A.ArrExp $1 $3 $5 }
      | LVal { A.VarExp $1 }
      | '-' Exp1 %prec negexp {% pos $ A.OpExp (A.IntExp 0) A.Minus $2 }


Arr1  : of Exp1  { $2 } -- わかりにくいので'(' ')' つけて良い気がする -- 怪しい
      -- | { (error "error:523") }

RecFs: id '=' Exp1 {% pos (\x -> [($1, $3, x)]) }
     | id '=' Exp1 ',' RecFs {% fmap (\x -> x:$5) $ pos (\x -> ($1, $3, x)) }
     |   { [] }

{

-- stubPos = AlexPn 0 0 0

-- 出題からは、FunDec, VarDec, FunDecはまとめなさそうな気がするが、意図としてはまとめて良さそうなのでそうする。
-- TODO: Decの型キモい。全体見えてきたら直す
reduceDecs :: [A.Dec] -> [A.Dec]
reduceDecs xs =
  let (fs, vs, ts) = foldr f ([], [], []) xs
  in concat[w ts A.TypeDecs, vs, w fs A.FunDecs]
  where
    f (A.FunDecs f) (fs, vs, ts) = (f ++ fs, vs, ts)
    f v@(A.VarDec _ _ _ _) (fs, vs, ts) = (fs, v:vs, ts)
    f (A.TypeDecs d) (fs, vs, ts) = (fs, vs, d ++ ts)
    w xs c = if null xs then [] else [c xs]

pos c = Alex $ \s -> Right (s, c (alex_pos s))

parseError :: TigToken -> Alex a
parseError x = error $ "Parse error: " ++ show x

main = do
  s <- getContents
  let r = runAlex s calc
  pPrint r

}
