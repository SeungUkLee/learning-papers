module Section2.RV1 where
import           Section2.Common

-- 2.7 Variation one, revisited: Exceptions

data M a
  = Raise Exception
  | Return a
  deriving (Show)

type Exception = String

unit :: a -> M a
unit = Return

andThen :: M a -> (a -> M b) -> M b
andThen m k = case m of
  Raise e  -> Raise e
  Return a -> k a

raise :: Exception -> M a
raise = Raise

eval :: Term -> M Int
eval (Con a)   = unit a
eval (Div t u) = eval t
  `andThen` \a -> eval u
  `andThen` \b -> if b == 0
    then raise "divide by zero"
    else unit (a `div` b)
