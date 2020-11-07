import           Data.List
import           Data.Maybe

type Id = String

data BinOp = Plus | Minus | Times | Div

data Stm = CompoundStm Stm Stm
  | AssignStm Id Exp
  | PrintStm [Exp]

data Exp = IdExp Id
  | NumExp Int
  | OpExp Exp BinOp Exp
  | EseqExp Stm Exp

prog =
 CompoundStm
  (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
  (CompoundStm
    (AssignStm "b" (EseqExp
                    (PrintStm [IdExp "a",OpExp (IdExp"a") Minus (NumExp 1)])
                    (OpExp (NumExp 10) Times (IdExp "a")))
    )
    (PrintStm [IdExp "b"])
  )

mmax Nothing Nothing = Nothing
mmax Nothing y       = y
mmax x Nothing       = x
mmax x y             = max x y

maxargs (CompoundStm x y) = mmax (maxargs x) (maxargs y)
maxargs (AssignStm _ e)   = maxe e
maxargs (PrintStm xs)     = Just $ length xs

maxe (EseqExp s e) = mmax (maxargs s) (maxe e)
maxe _             = Nothing

type Table = [(Id, Int)]

lookup' k xs = fromJust $ lookup k xs

interp env (CompoundStm x y) =  interp (interp env x) y
interp env (AssignStm id e)  =
  let ((t', ps'), x) = interpExp env e
  in ((id, x):t', ps')
interp env@(t, ps) (PrintStm xs) =
  foldl f env xs
  where f env e =
          let ((t, ps), x) = interpExp env e
          in (t, (show x):ps)

interpExp env@(t, _) (IdExp id) = (env, lookup' id t)
interpExp env (NumExp x) = (env, x)
interpExp env (OpExp e1 op e2) =
  let (env', x) = interpExp env e1 -- ?
      (env'', y) = interpExp env' e2
  in (env'', evalOp x op y)
interpExp env@(t, _) (EseqExp s e) =
  let env' = interp env s
  in interpExp env' e

evalOp :: Int -> BinOp -> Int -> Int
evalOp x Plus y  = x + y
evalOp x Minus y = x - y
evalOp x Div y   = x `div` y
evalOp x Times y = x * y

main' = mapM_ putStrLn . reverse . snd . interp ([], [])
