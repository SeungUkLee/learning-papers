module TypeInferencer
    ( Exp (..), Lit (..), Type (..), Scheme (..), typeInference, runTI
    ) where

import qualified Data.Map             as Map
import qualified Data.Set             as Set
import qualified Text.PrettyPrint     as PP

import           Control.Monad.Except
-- import           Control.Monad.Reader
import           Control.Monad.State

data Exp
  = EVar String
  | ELit Lit
  | EApp Exp Exp
  | EAbs String Exp
  | ELet String Exp Exp

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Eq, Ord)

data Type
  = TVar String
  | TInt
  | TBool
  | TFun Type Type
  deriving (Eq, Ord)

data Scheme = Scheme [String] Type

class Types a where
  ftv :: a -> Set.Set String
  apply :: Subst -> a -> a

instance Types Type where
  ftv (TVar n)     = Set.singleton n
  ftv TInt         = Set.empty
  ftv TBool        = Set.empty
  ftv (TFun t1 t2) = Set.union (ftv t1) (ftv t2)
  apply s (TVar n) =
    case Map.lookup n s of
      Nothing -> TVar n
      Just t  -> t
  apply s (TFun t1 t2) = TFun (apply s t1) (apply s t2)
  apply _ t            = t

instance Types Scheme where
  ftv (Scheme vars t) = Set.difference (ftv t) (Set.fromList vars)
  apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t)

instance Types a => Types [a] where
  apply s = map (apply s)
  -- ftv l = foldr Set.union (Set.empty) (map ftv l)
  ftv = foldr (Set.union . ftv) Set.empty

type Subst = Map.Map String Type
nullSubst :: Subst
nullSubst = Map.empty

-- composeSubst :: Subst -> Subst -> Subst
-- composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1
(@@) :: Subst -> Subst -> Subst
(@@) s1 s2 = Map.map (apply s1) s2 `Map.union` s1

newtype TypeEnv = TypeEnv (Map.Map String Scheme)

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

instance Types TypeEnv where
  ftv (TypeEnv env) = ftv (Map.elems env)
  apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
  where vars = Set.toList (ftv t `Set.difference` ftv env)

-- data TIEnv = TIEnv {}
type TIState = Int
type TI a = ExceptT String (State TIState) a

runTI :: TI a -> (Either String a, TIState)
runTI t = runState (runExceptT t) initTIState
  where initTIState = 0

newTyVar :: TI Type
newTyVar = do
  s <- get
  put (s + 1)
  return (TVar (reverse (toTyVar s)))
  where
    toTyVar :: Int -> String
    toTyVar c | c < 26 = [toEnum (97 + c)]
      | otherwise = let (n, r) = c `divMod` 26
        in toEnum (97 + r) : toTyVar (n - 1)

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
  nvars <- mapM (const newTyVar) vars
  let s = Map.fromList (zip vars nvars)
  return $ apply s t

mgu :: Type -> Type -> TI Subst
mgu (TFun l r) (TFun l' r') = do
  s1 <- mgu l l'
  s2 <- mgu (apply s1 r) (apply s1 r')
  return (s1 @@ s2)
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu TInt TInt = return nullSubst
mgu TBool TBool = return nullSubst
mgu t1 t2 = throwError $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2

varBind :: String -> Type -> TI Subst
varBind u t
  | t == TVar u = return nullSubst
  | u `Set.member` ftv t = throwError $ "occurs check fails: " ++ u ++ "vs. " ++ show t
  | otherwise = return (Map.singleton u t)

tiLit :: TypeEnv -> Lit -> TI (Subst, Type)
tiLit _ (LInt _)  = return (nullSubst, TInt)
tiLit _ (LBool _) = return (nullSubst, TBool)

ti :: TypeEnv -> Exp -> TI (Subst, Type)
ti (TypeEnv env) (EVar n) =
  case Map.lookup n env of
    Nothing -> throwError $ "unbound variable: " ++ n
    Just sigma -> do
      t <- instantiate sigma
      return (nullSubst, t)
ti env (ELit l) = tiLit env l
ti env (EAbs n e) = do
  tv <- newTyVar
  let TypeEnv env' = remove env n
  let env'' = TypeEnv (env' `Map.union` Map.singleton n (Scheme [] tv))
  (s1, t1) <- ti env'' e
  return (s1, TFun (apply s1 tv) t1)
ti env (EApp e1 e2) = do
  tv <- newTyVar
  (s1, t1) <- ti env e1
  (s2, t2) <- ti (apply s1 env) e2
  s3 <- mgu (apply s2 t1) (TFun t2 tv)
  return (s3 @@ s2 @@ s1, apply s3 tv)
ti env (ELet x e1 e2) = do
  (s1, t1) <- ti env e1
  let TypeEnv env' = remove env x
  let t' = generalize (apply s1 env) t1
  let env'' = TypeEnv (Map.insert x t' env')
  (s2, t2) <- ti (apply s1 env'') e2
  return (s1 @@ s2, t2)

typeInference :: Map.Map String Scheme -> Exp -> TI Type
typeInference env e = do
  (s, t) <- ti (TypeEnv env) e
  return (apply s t)


instance Show Type where
  showsPrec _ x = shows (prType x)

prType :: Type -> PP.Doc
prType (TVar n)   = PP.text n
prType TInt       = PP.text "Int"
prType TBool      = PP.text "Bool"
prType (TFun t s) = prParenType t PP.<+> PP.text "->" PP.<+> prType s

prParenType :: Type -> PP.Doc
prParenType t = case t of
  TFun _ _ -> PP.parens (prType t)
  _        -> prType t

instance Show Exp where
  showsPrec _ x = shows (prExp x)

prExp :: Exp -> PP.Doc
prExp (EVar name) = PP.text name
prExp (ELit lit) = prLit lit
prExp (ELet x b body) =
  PP.text "let" PP.<+>
  PP.text x PP.<+> PP.text "=" PP.<+>
  prExp b PP.<+> PP.text "in" PP.$$
  PP.nest 2 (prExp body)
prExp (EApp e1 e2) = prExp e1 PP.<+> prParenExp e2
prExp (EAbs n e) =
  PP.char '\\' PP.<+> PP.text n PP.<+>
  PP.text "->" PP.<+>
  prExp e

prParenExp :: Exp -> PP.Doc
prParenExp t = case t of
  ELet {}  -> PP.parens (prExp t)
  EApp _ _ -> PP.parens (prExp t)
  EAbs _ _ -> PP.parens (prExp t)
  _        -> prExp t

instance Show Lit where
  showsPrec _ x = shows (prLit x)

prLit :: Lit -> PP.Doc
prLit (LInt i)  = PP.integer i
prLit (LBool b) = if b then PP.text "True" else PP.text "False"

instance Show Scheme where
  showsPrec _ x = shows (prScheme x)

prScheme :: Scheme -> PP.Doc
prScheme (Scheme vars t) =
  PP.text "All" PP.<+>
  PP.hcat (PP.punctuate PP.comma (map PP.text vars))
  PP.<> PP.text "." PP.<+> prType t
