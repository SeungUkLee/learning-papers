module Section3.Laws where

-- 3 Monad laws

data Term
  = Con Int
  | Add Term Term

type M a = a

unit :: a -> M a
unit a = a

andThen :: M a -> (a -> M b) -> M b
a `andThen` k = k a

eval :: Term -> M Int
eval (Con a) = unit a
eval (Add t u) =  eval t
  `andThen` \a -> eval u
  `andThen` \b -> unit (a + b)

e1 = eval (Add (Con 1) (Add (Con 2) (Con 3))) -- eval (Add t (Add u v))
e2 = eval (Add (Add (Con 1) (Con 2)) (Con 3)) -- eval (Add (Add t u) v)

map' :: (a -> b) -> (M a -> M b)
map' f m = m `andThen` \a -> unit (f a)

join' :: M (M a) -> M a
join' z = z `andThen` id
