{
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Main (main) where

import Prelude hiding (GT, LT)
import Control.Monad
import Debug.Trace
}

%wrapper "monadUserState"

@digits = [0-9]+
@real = @digits \. [0-9]* | [0-9]* \. @digits
@identifier = [a-zA-Z][a-zA-Z0-9_]*

tokens :-

<0>          $white+     ;
<0>          "--".*      ;
<0>          if          {\(p, _, _, _) _ -> pure $ (p, If)}
<0>          "-"         { mkToken Negation }
<0>          "("         { mkToken LParen }
<0>          ")"         { mkToken RParen }
<0>          "{"         { mkToken LBrace }
<0>          "}"         { mkToken RBrace }
<0>          "["         { mkToken LBracket }
<0>          "]"         { mkToken RBracket }
<0>          ","         { mkToken Comma }
<0>          ":"         { mkToken Colon }
<0>          ";"         { mkToken Semicolon }
<0>          "type"      { mkToken Type }
<0>          "then"      { mkToken Then }
<0>          "else"      { mkToken Else }
<0>          "var"       { mkToken Var      }
<0>          "function"  { mkToken Function }
<0>          "let"       { mkToken Let      }
<0>          "in"        { mkToken In       }
<0>          "end"       { mkToken End      }
<0>          ":="      { mkToken Bind     }
<0>          "array"     { mkToken Array    }
<0>          "of"        { mkToken Of       }
<0>          "while"     { mkToken While    }
<0>          "for"       { mkToken For      }
<0>          "to"        { mkToken To       }
<0>          "break"     { mkToken Break    }
<0>          "do"        { mkToken Do       }
<0>          "nil"       { mkToken Nil      }
<0>          "+"       { mkToken Plus  }
<0>          "*"       { mkToken Times  }
<0>          "/"       { mkToken Div   }
<0>          "="       { mkToken Eq    }
<0>          "<>"       { mkToken NotEq }
<0>          "<"       { mkToken LT    }
<0>          "<="       { mkToken LTE   }
<0>          ">"       { mkToken GT    }
<0>          ">="       { mkToken GTE   }
<0>          "&"       { mkToken And   }
<0>          "|"       { mkToken Or    }
<0>          "."       { mkToken Dot    }
<0>          @identifier { \(p, _, _, str) len -> pure $ (p, Id (take len str))}
<0>          @digits     { \(p, _, _, str) len -> pure $ (p, Num (read $ take len str)) }
<0>          @real       {\(p, _, _, s) l -> pure $ (p, Real (readReal $ take l s))}
<0>          \"          { beginString }
<string>     \"          { endString }
<string>     \\\"        { \i s -> addString "\"" >> skip i s }
<string>     .           { takeString }
<0, comment> "/*"        { beginComment }
<comment>    "*/"        { endComment }
<comment>    .           ;
<comment>    \n          ;

{

mkToken :: Token -> AlexInput -> Int -> Alex (AlexPosn, Token)
mkToken c (p, _, _, _) _ = pure $ (p, c)
-- mkToken' c (p, _, _, s) l = pure $ (p, c (take l s))

-- The token type:
data Token =
  EOF
  | Id String
  | Num Int
  | Real Double
  | StringL String
  | Negation
  | LParen
  | RParen
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  | Comma
  | Colon
  | Semicolon
  | Type
  | If
  | Then
  | Else
  | Var
  | Function
  | Let
  | In
  | End
  | Bind
  | Array
  | Of
  | While
  | For
  | To
  | Break
  | Do
  | Nil
  | Plus
  | Times
  | Div
  | Eq
  | NotEq
  | LT
  | LTE
  | GT
  | GTE
  | And
  | Or
  | Dot
  deriving (Eq,Show)

readReal s
  | head s == '.' = read ('0':s)
  | last s == '.' = read (s ++ "0")
  | otherwise = read s

data AlexUserState = AlexUserState
  { commentDepth :: Int
  , stringBuf    :: Maybe (AlexPosn, String)
  }
alexInitUserState = AlexUserState
  { commentDepth = 0
  , stringBuf = Nothing
  }

beginString i@(p, _, _, _) l = do
  Alex $ \s@AlexState{alex_ust=ust} -> Right (s{alex_ust=ust{stringBuf=Just (p, "")}}, ())
  alexSetStartCode string
  alexMonadScan

addString s = do
  Alex $ \st@AlexState{alex_ust=ust} -> case stringBuf ust of
    Just (p, str) ->
      Right (st{alex_ust=ust{stringBuf=Just (p, str ++ s)}}, ())
    _ -> error "impossible"

takeString i@(_, _, _, s) l = do
  addString $ take l s
  skip i l

endString i l = do
  buf <- Alex $ \s@AlexState{alex_ust=ust} -> Right (s, stringBuf ust)
  case buf of
    Just (p, str) -> do
      alexSetStartCode 0
      pure $ (p, StringL str)
    _ -> error "impossible"

beginComment i l = do
  d <- getCommentDepth
  setCommentDepth (d + 1)
  alexSetStartCode comment
  skip i l

endComment i l = do
  d <- getCommentDepth
  setCommentDepth (d - 1)
  when (d == 1) $ alexSetStartCode 0
  skip i l

getCommentDepth :: Alex Int
getCommentDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, commentDepth ust)

setCommentDepth :: Int -> Alex ()
setCommentDepth ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){commentDepth=ss}}, ())

scanner :: String -> Either String [(AlexPosn, Token)]
scanner str = runAlex str $ do
  let loop toks = do
        tok <- alexMonadScan
        cd <- getCommentDepth
        if snd (trace (show (tok, cd)) tok) == EOF then
          return toks
	  else let toks' = tok:toks in toks' `seq` loop toks'
  loop []

alexEOF = return (AlexPn (-1) 0 0, EOF)

main = do
 s <- getContents
 case scanner s of
   Right xs -> print $ reverse xs
   Left e -> error ( show e)
}
