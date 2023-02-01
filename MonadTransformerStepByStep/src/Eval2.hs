module Eval2 where

import           Language

import           Control.Monad.Except
import           Control.Monad.Identity
import qualified Data.Map               as Map

type Eval2 a = ExceptT String Identity a

runEval2 :: Eval2 a -> Either String a
runEval2 ev = runIdentity (runExceptT ev)

eval2a :: Env -> Exp -> Eval2 Value
eval2a env (Lit i) = return $ IntVal i
eval2a env (Var n) = case Map.lookup n env of
  Nothing  -> error ("Unbound variable: " ++ n)
  Just val -> return val
eval2a env (Plus e1 e2) = do
  ~(IntVal i1) <- eval2a env e1
  ~(IntVal i2) <- eval2a env e2
  return $ IntVal (i1 + i2)
eval2a env (Abs n e) = return $ FunVal env n e
eval2a env (App e1 e2) = do
  val1 <- eval2a env e1
  val2 <- eval2a env e2
  case val1 of
    FunVal env' n body -> eval2a (Map.insert n val2 env') body

eval2b :: Env -> Exp -> Eval2 Value
eval2b env (Lit i) = return $ IntVal i
eval2b env (Var n) = case Map.lookup n env of
  Nothing  -> throwError ("Unbound variable: " ++ n)
  Just val -> return val
eval2b env (Plus e1 e2) = do
  e1' <- eval2b env e1
  e2' <- eval2b env e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _                      -> throwError "type error"
eval2b env (Abs n e) = return $ FunVal env n e
eval2b env (App e1 e2) = do
  val1 <- eval2b env e1
  val2 <- eval2b env e2
  case val1 of
    FunVal env' n body -> eval2b (Map.insert n val2 env') body
    _                  -> throwError "type error"

eval2c :: Env -> Exp -> Eval2 Value
eval2c env (Lit i) = return $ IntVal i
eval2c env (Var n) = case Map.lookup n env of
  Nothing  -> throwError ("Unbound variable: " ++ n)
  Just val -> return val
eval2c env (Plus e1 e2) = do
  ~(IntVal i1) <- eval2c env e1
  ~(IntVal i2) <- eval2c env e2
  return $ IntVal (i1 + i2)
eval2c env (Abs n e) = return $ FunVal env n e
eval2c env (App e1 e2) = do
  ~(FunVal env' n body) <- eval2c env e1
  val2 <- eval2c env e2
  eval2c (Map.insert n val2 env') body

eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i) = return $ IntVal i
eval2 env (Var n) = case Map.lookup n env of
  Nothing  -> throwError ("Unbound variable: " ++ n)
  Just val -> return val
eval2 env (Plus e1 e2) = do
  e1' <- eval2 env e1
  e2' <- eval2 env e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _                      -> throwError "type error in addition"
eval2 env (Abs n e) = return $ FunVal env n e
eval2 env (App e1 e2) = do
  val1 <- eval2b env e1
  val2 <- eval2b env e2
  case val1 of
    FunVal env' n body -> eval2b (Map.insert n val2 env') body
    _                  -> throwError "type error in application"
