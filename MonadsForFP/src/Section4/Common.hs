module Section4.Common
  ( Term(..), Comm(..), Prog(..), State, Ix, Val, Id, Arr, newarray, index, update
  ) where

data Term
  = Var Id
  | Con Int
  | Add Term Term

data Comm
  = Asgn Id Term
  | Seq Comm Comm
  | If Term Comm Comm

data Prog
  = Prog Comm Term

type State = Arr
type Ix = Id
type Val = Int

type Id = Int
type Arr = String

newarray :: Val -> Arr
newarray = undefined

index :: Ix -> Arr -> Val
index = undefined

update :: Ix -> Val -> Arr -> Arr
update = undefined
