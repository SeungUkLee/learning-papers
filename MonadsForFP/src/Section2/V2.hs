module Section2.V2 where

import           Section2.Common

-- 2.3 Variation two: State

type M a = State -> (a, State)
type State = Int

eval :: Term -> M Int
eval (Con a) x = (a, x)
eval (Div t u) x = let (a, y) = eval t x in
  let (b, z) = eval u y in
  (a `div` b, z + 1)
