module Section2.Common
  ( Term(..), answer, err
  ) where

data Term
  = Con Int
  | Div Term Term
  deriving (Show)

answer :: Term
answer = Div (Div (Con 1972) (Con 2)) (Con 23)

err :: Term
err = Div (Con 1) (Con 0)
