module Section2.V1 where
import           Section2.Common

-- 2.2 Variation one: Exception

data M a
  = Raise Exception
  | Return a
  deriving (Show)

type Exception = String

eval :: Term -> M Int
eval (Con a) = Return a
eval (Div t u) = case eval t of
  Raise e -> Raise e
  Return a ->
    case eval u of
      Raise e -> Raise e
      Return b ->
        if b == 0
          then Raise "divide by zero"
          else Return (a `div` b)
