module Main where
import Language

import qualified Data.Map   as Map
import Eval2
import Eval3
import Eval1
import Eval6

main = do
  e6
  return ()

exampleExp = Lit 12 `Plus` App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2)
errorExp = Plus (Lit 1) (Abs "x" (Var "x"))


e1 = runEval1 (eval1 Map.empty exampleExp)
e2_a = runEval2 (eval2a Map.empty exampleExp)


e3_b = runEval2 (eval2b Map.empty errorExp)
-- Left "type error"

e3 = runEval3 Map.empty (eval3 exampleExp)

e6 = runEval6 Map.empty 0 (eval6 exampleExp)


