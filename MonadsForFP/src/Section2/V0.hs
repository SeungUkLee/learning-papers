module Section2.V0 where
import           Section2.Common

-- 2.1 Variation zero: The basic evaluator

eval :: Term -> Int
eval (Con a)   = a
eval (Div t u) = eval t `div` eval u
