{
module Main where

import Prelude hiding (GT, LT)
}

%name calc
%tokentype { Char }
%error { parseError }

%token
    a         { 'a' }
    b         { 'b' }

-- %left aexp
-- %left b

%%

-- E: {}
--  | x Y x {}
--  | y X y {}
-- Y: y Y {}
--  |  {}
-- X: x X {}
--  |  {}
--
-- E: Ea {}
-- | Eb {}
-- Ea: B1 '.' B0 {}
-- Eb: '.' B1 {}
-- B: '0' {}
--  | '1' {}
-- B0: B B0 {}
--  | {}
-- B1: B B1 {}
--  | B {}


E: a E a {}
 | b E b  {}
 | {}

-- E: a E1 b {}
--  | a E {}
--  | {}
-- E1: a E1 b {}
--   | {}
--
{

parseError :: String -> a
parseError xs = error $ "Parse error: " ++ show xs

--data Token= XToken | YToken
--  deriving (Show)

main = do
  print $ calc "abba"
}
