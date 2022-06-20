module Section4.A1 where

import           Section4.Common

-- 4.1 Arrays

eval :: Term -> State -> Int
eval (Var i) x   = index i x
eval (Con a) x   = a
eval (Add t u) x = eval t x + eval u x

exec :: Comm -> State -> State
exec (Asgn i t) x = update i (eval t x) x
exec (Seq c d) x  = exec d (exec c x)
exec (If t c d) x = if eval t x == 0 then exec c x else exec d x

elab :: Prog -> Int
elab (Prog c t) = eval t (exec c (newarray 0))
