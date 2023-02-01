module Language (Exp (..), Name, Value (..), Env) where

import qualified Data.Map as Map

-- The following data types for modeling programs in that language will be used:

data Exp -- expressions
  = Lit Integer -- literal integers (constants)
  | Var Name  -- variables
  | Plus Exp Exp  -- addition
  | Abs Name Exp  -- lambda expression (abstractions)
  | App Exp Exp -- functional application
  deriving (Show)

type Name = String -- variable names

data Value -- values
  = IntVal Integer  -- integers
  | FunVal Env Name Exp -- functions (closuers)
  deriving (Show)

-- The Env component of a FunVal is the environment in which the corresponding λ-abstraction was evaluated.
type Env = Map.Map Name Value -- mapping from names to values
