module Section2.RV0 where
import           Section2.Common

-- 2.6 Variation zero, revisited: The basic evaluator

type M a = a

unit :: a -> M a
unit a = a

andThen :: M a -> (a -> M b) -> M b
a `andThen` k = k a

eval :: Term -> M Int
eval (Con a)   = unit a
eval (Div t u) = eval t
  `andThen` \a -> eval u
  `andThen` \b -> unit (a `div` b)
