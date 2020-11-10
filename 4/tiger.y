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

%token
    if          { (_, If) }
    '-'         { (_, Negation) }
    '('         { (_, LParen) }
    ')'         { (_, RParen) }
    '{'         { (_, LBrace) }
    '}'         { (_, RBrace) }
    '['         { (_, LBracket) }
    ']'         { (_, RBracket) }
    ','         { (_, Comma) }
    ':'         { (_, Colon) }
    ';'         { (_, Semicolon) }
    type      { (_, Type) }
    then      { (_, Then) }
    else      { (_, Else) }
    var       { (_, Var)      }
    function  { (_, Function) }
    let       { (_, Let)      }
    in        { (_, In)       }
    end       { (_, End)      }
    ':='        { (_, Bind)     }
    array     { (_, Array)    }
    of        { (_, Of)       }
    while     { (_, While)    }
    for       { (_, For)      }
    to        { (_, To)       }
    break     { (_, Break)    }
    do        { (_, Do)       }
    nil       { (_, Nil)      }
    '+'         { (_, Plus)  }
    '*'         { (_, Times)  }
    '/'         { (_, Div)   }
    '='         { (_, Eq)    }
    '<>'        { (_, NotEq) }
    '<'         { (_, LT)    }
    '<='        { (_, LTE)   }
    '>'         { (_, GT)    }
    '>='        { (_, GTE)   }
    '&'         { (_, And)   }
    '|'         { (_, Or)    }
    '.'         { (_, Dot)    }
    id          { (_, Id $$) }
    num         { (_, Num $$) }
    real        { (_, Real $$) }
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

Exp   : let Decs in Seq end  { (A.LetExp (snd $2) $4 (fst $1), fst $1) }
      | Exp1 %prec atomexp { $1 }

Decs  : Dec Decs                { (error "0") }
      | Dec                     { (error "1") }

Dec   : TyDec                   { (error "2") }
      | VarDec                  { (error "3") }
      | FunDec                  { (error "4") }

TyDec : type TyId '=' Ty          { (error "5") }

TyId  : id                      { (error "6") }

Ty    : TyId                    { (error "7") }
      | array of TyId           { (error "8") }
      | '{' TyFs '}' { (error "9") }

TyFs  : id ':' TyId ',' TyFs        { (error "10") }
      | id ':' TyId                 { (error "11") }

VarDec: var id ':=' Exp           { (error "12") }
      | var id ':' id ':=' Exp  { (error "13") }

FunDec: function id '(' TyFs ')' '=' Exp1 { (error "14") }
      | function id '(' TyFs ')' ':' TyId '=' Exp1 { (error "15") }

Seq   : Exp { A.SeqExp [$1] }
      | Exp ';' Seq { A.SeqExp ((\(A.SeqExp xs) -> $1:xs) $3) }

LVal  : id ValAcc { A.SimpleVar $1 stubPos  }

ValAcc: '.' id ValAcc { (error "18") }
      | '[' Exp1 ']' ValAcc { (error "19") }
      |   { A.NilExp } -- tmp

Exp1  : Exp1 '+' Exp1           { (A.OpExp (fst $1) A.Plus (fst $3) (fst $2), fst $2) }
      | Exp1 '-' Exp1           { (error "20") }
      | Exp1 '*' Exp1           { (error "21") }
      | Exp1 '/' Exp1           { (error "22") }
      | Exp1 '&' Exp1           { (error "23") }
      | Exp1 '|' Exp1           { (error "24") }
      | Exp1 '=' Exp1 { (error "25") }
      | Exp1 '<>' Exp1 { (error "26") }
      | Exp1 '>' Exp1 { (error "27") }
      | Exp1 '<' Exp1 { (error "28") }
      | Exp1 '>=' Exp1 { (error "29") }
      | Exp1 '<=' Exp1 { (error "30") }
      | LVal ':=' Exp1 { (A.AssignExp $1 (fst $3) (fst $2), fst $2) }
      | Atom                    { ($1, stubPos) }
      | id '(' Vals ')' { (error "32") }
      | id '(' ')' { (error "33") }
      | '(' Seq ')'             { ($2, fst $1) }
      | '(' ')' { (error "34") }
      | while Exp1 do Exp { (error "35") }
      | for id ':=' Exp1 to Exp1 do Exp { (error "36") }
      | if Exp1 then Exp %prec ifexp { (error "37") }
      | if Exp1 then Exp else Exp { (error "38") }
      | break { (error "39") }

Vals  : Exp1 { (error "40") }
      | Exp1 ',' Vals { (error "41") }

Atom
      : num                     { A.IntExp $1 }
      | real                     { (error "43") }
      | str { (error "44") }
      | id { A.VarExp (A.SimpleVar ($1) stubPos) }
      | nil { (error "46") }
      | TyId '{' RecFs '}' { (error "47") }
      | id '.' id { (error "48") }
      | id '[' Exp1 ']' Arr1 { (error "49") }
      | '-' Exp1 %prec negexp { (error "50") }

Arr1  : of Exp1  { (error "51") } -- わかりにくいので'(' ')' つけて良い気がする -- 怪しい
      | { (error "52") }

RecFs: id '=' Exp1 { (error "53") }
     | id '=' Exp1 ',' RecFs { (error "54") }
     |   { (error "55") }

{

stubPos = AlexPn 0 0 0

parseError :: [TigToken] -> a
parseError xs = error $ "Parse error: " ++ show xs

main = getContents >>= pPrint . calc . lexer

}
