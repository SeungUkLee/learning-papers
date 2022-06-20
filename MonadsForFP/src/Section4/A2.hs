module Section4.A2 where

import           Section4.Common (Arr, Comm (..), Ix, Prog (..), Term (..), Val,
                                  index, newarray, update)

-- 4.2 Array transformers

type M a = State -> (a, State)
type State = Arr

unit :: a -> M a
unit a x = (a, x)

andThen :: M a -> (a -> M b) -> M b
m `andThen` k = \x -> let (a, y) = m x in
  let (b, z) = k a y in
  (b, z)

block :: Val -> M a -> a
block v m = let (a, x) = m (newarray v) in a

fetch :: Ix -> M Val
fetch i x = (index i x, x)

assign :: Ix -> Val -> M ()
assign i v x = ((), update i v x)

eval :: Term -> M Int
eval (Var i) = fetch i
eval (Con a) = unit a
eval (Add t u) = eval t
  `andThen` \a -> eval u
  `andThen` \b -> unit (a + b)

exec :: Comm -> M ()
exec (Asgn i t) = eval t
  `andThen` \a -> assign i a
exec(Seq c d) = exec c
  `andThen` \() -> exec d
  `andThen` \() -> unit ()
exec (If t c d) = eval t
  `andThen` \a ->
    if a == 0 then exec c else exec d

elab :: Prog -> Int
elab (Prog c t) = block 0 (exec c `andThen` \() -> eval t `andThen` \a -> unit a)
