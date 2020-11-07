{
module Main where

import Prelude hiding (GT, LT)
import Lexer
}

%name calc
%tokentype { Token }
%error { parseError }

%token
    if          { If }
    '-'         { Negation }
    '('         { LParen }
    ')'         { RParen }
    '{'         { LBrace }
    '}'         { RBrace }
    '['         { LBracket }
    ']'         { RBracket }
    ','         { Comma }
    ':'         { Colon }
    ';'         { Semicolon }
    type      { Type }
    then      { Then }
    else      { Else }
    var       { Var      }
    function  { Function }
    let       { Let      }
    in        { In       }
    end       { End      }
    ':='        { Bind     }
    array     { Array    }
    of        { Of       }
    while     { While    }
    for       { For      }
    to        { To       }
    break     { Break    }
    do        { Do       }
    nil       { Nil      }
    '+'         { Plus  }
    '*'         { Times  }
    '/'         { Div   }
    '='         { Eq    }
    '<>'        { NotEq }
    '<'         { LT    }
    '<='        { LTE   }
    '>'         { GT    }
    '>='        { GTE   }
    '&'         { And   }
    '|'         { Or    }
    '.'         { Dot    }
    id          { Id $$ }
    num         { Num $$ }
    real        { Real $$ }
    str         { StringL $$ }

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

Exp   : let Decs in Seq end  {  }
      | Exp1 %prec atomexp {}

Decs  : Dec Decs                {}
      | Dec                     {}

Dec   : TyDec                   {}
      | VarDec                  {}
      | FunDec                  {}

TyDec : type TyId '=' Ty          {}

TyId  : id                      {}

Ty    : TyId                    {}
      | array of TyId           {}
      | '{' TyFs '}' {}

TyFs  : id ':' TyId ',' TyFs        {}
      | id ':' TyId                 {}

VarDec: var id ':=' Exp           {}
      | var id ':' id ':=' Exp  {}

FunDec: function id '(' TyFs ')' '=' Exp1 {}
      | function id '(' TyFs ')' ':' TyId '=' Exp1 {}

Seq   : Exp {}
      | Exp ';' Seq {}

LVal  : id ValAcc {}

ValAcc: '.' id ValAcc {}
      | '[' Exp1 ']' ValAcc {}
      |   {}

Exp1  : Exp1 '+' Exp1           {  }
      | Exp1 '-' Exp1           {  }
      | Exp1 '*' Exp1           {  }
      | Exp1 '/' Exp1           {  }
      | Exp1 '&' Exp1           {  }
      | Exp1 '|' Exp1           {  }
      | Exp1 '=' Exp1 {}
      | Exp1 '<>' Exp1 {}
      | Exp1 '>' Exp1 {}
      | Exp1 '<' Exp1 {}
      | Exp1 '>=' Exp1 {}
      | Exp1 '<=' Exp1 {}
      | LVal ':=' Exp1 {}
      | Atom                    {  }
      | id '(' Vals ')' {}
      | id '(' ')' {}
      | '(' Seq ')'             {  }
      | '(' ')' {}
      | while Exp1 do Exp {}
      | for id ':=' Exp1 to Exp1 do Exp {}
      | if Exp1 then Exp %prec ifexp {}
      | if Exp1 then Exp else Exp {}
      | break {}

Vals  : Exp1 {}
      | Exp1 ',' Vals {}

Atom
      : num                     {  }
      | real                     {  }
      | str {}
      | id {}
      | nil {}
      | TyId '{' RecFs '}' {}
      | id '.' id {}
      | id '[' Exp1 ']' Arr1 {}
      | '-' Exp1 %prec negexp {}

Arr1  : of Exp1  {} -- わかりにくいので'(' ')' つけて良い気がする -- 怪しい
      | {}

RecFs: id '=' Exp1 {}
     | id '=' Exp1 ',' RecFs {}
     |   {}

{

parseError :: [Token] -> a
parseError xs = error $ "Parse error: " ++ show xs

main = getContents >>= print . calc . map snd . lexer

-- 意味動作なしで構文解析だけ作るの難しくないか？
-- -> -iと-a -dオプション
}
