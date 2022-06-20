module Section2.V3 where

import           Section2.Common

-- 2.4 Variation three: Output

type M a = (Output, a)
type Output = String

eval :: Term -> M Int
eval (Con a) = (line (Con a) a, a)
eval (Div t u) = let (x, a) = eval t in
  let (y, b) = eval u in
  (x ++ y ++ line (Div t u)(a `div` b), a `div` b)

line :: Term -> Int -> Output
line t a = "eval (" ++ show t ++ ") <= " ++ show a ++ "\n"
