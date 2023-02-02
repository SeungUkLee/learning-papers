module Main (main) where

import TypeInferencer
import qualified Data.Map             as Map

e0 = ELet "id" (EAbs "x" (EVar "x")) (EVar "id")
e1 = ELet "id" (EAbs "x" (EVar "x")) (EApp (EVar "id") (EVar "id"))
e2 = ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y"))) (EApp (EVar "id") (EVar "id"))
e3 = ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y"))) (EApp (EApp (EVar "id") (EVar "id")) (ELit (LInt 2)))
e4 = ELet "id" (EAbs "x" (EApp (EVar "x") (EVar "x"))) (EVar "id")
e5 = EAbs "m" (ELet "y" (EVar "m") (ELet "x" (EApp (EVar "y") (ELit (LBool True))) (EVar "x")))

test :: Exp -> IO ()
test e =
  let (res, _) = runTI (typeInference Map.empty e)
  in case res of
    Left err -> putStrLn $ show e ++ "\nerror: " ++ err
    Right t  -> putStrLn $ show e ++ " :: " ++ show t


main :: IO ()
main = mapM_ test [e0, e1, e2, e3, e4, e5]
