module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data List a = Nil | Cons a (List a)

sumList :: Num p => List p -> p
sumList Nil = 0
sumList (Cons n l) = n + sumList l

productList :: Num p => List p -> p
productList Nil = 1
productList (Cons n l) = n * productList l

listFoldr :: (a -> b -> b) -> b -> List a -> b
(listFoldr f x) Nil = x
(listFoldr f x) (Cons a l) = f a (listFoldr f x l)

productList2 :: Num p => List p -> p
productList2 = listFoldr (*) 1

anytrue :: List Bool -> Bool
anytrue = listFoldr (||) False

alltrue :: List Bool -> Bool
alltrue = listFoldr (&&) True

listAppend :: List a -> List a -> List a
listAppend a b = listFoldr Cons b a

listLength :: List a -> Integer
listLength = listFoldr count 0 where count a n = n + 1

doubleall :: List Integer -> List Integer
doubleall = listFoldr doubleandcons Nil where doubleandcons n list = Cons (2 * n) list
