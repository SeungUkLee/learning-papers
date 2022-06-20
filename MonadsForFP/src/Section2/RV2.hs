module Section2.RV2 where

import           Section2.Common

-- 2.8 Variation two, revisited: State

type M a = State -> (a, State)
type State = Int

unit :: a -> M a
unit a x = (a, x)

andThen :: M a -> (a -> M b) -> M b
andThen m k x = let (a, y) = m x in
  let (b, z) = k a y in
  (b ,z)

tick :: M ()
tick x = ((), x + 1)

eval :: Term -> M Int
eval (Con a)   = unit a
eval (Div t u) = eval t
  `andThen` \a -> eval u
  `andThen` \b -> tick `andThen` \() -> unit (a `div` b)
