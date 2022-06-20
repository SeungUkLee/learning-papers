module Section2.RV3 where
import           Section2.Common

-- 2.9 Variation three, revisited: Output

type M a = (Output, a)
type Output = String

unit :: a -> M a
unit a = ("", a)

andThen :: M a -> (a -> M b) -> M b
andThen m k = let (x, a) = m in
  let (y, b) = k a in
  (x ++ y, b)

out :: Output -> M ()
out x = (x, ())

line :: Term -> Int -> Output
line t a = "eval (" ++ show t ++ ") <= " ++ show a ++ "\n"

eval :: Term -> M Int
eval (Con a)   = out (line (Con a) a) `andThen` \() -> unit a
eval (Div t u) = eval t
  `andThen` \a -> eval u
  `andThen` \b -> out (line (Div t u) (a `div` b)) `andThen` \() -> unit (a `div` b)
