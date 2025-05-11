module TypeChecker where

import AST
import Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Type environment (maps variables to their types)
type TypeEnv = Map.Map VarId Type

-- Type inference state monad for generating fresh type variables
type InferState a = State Int a

-- Generate a fresh type variable
getFreshTVar :: InferState Type
getFreshTVar = do
  n <- get
  put (n + 1)
  return (TVar n)

-- Infer types and generate constraints
infer :: TypeEnv -> Expr -> InferState (Type, ConstraintSet)
infer g (CInt _) = return (TInt, Set.empty)
infer g (CBool _) = return (TBool, Set.empty)
infer g (Var varId) = 
  case Map.lookup varId g of
    Nothing -> return (TError, Set.singleton CError)
    Just t  -> return (t, Set.empty)
    
infer g (Plus e1 e2) = do 
  (t1, c1) <- infer g e1
  (t2, c2) <- infer g e2
  let c = Set.union c1 c2
  return (TInt, Set.insert (CEq t1 TInt) (Set.insert (CEq t2 TInt) c))
  
infer g (Minus e1 e2) = do
  (t1, c1) <- infer g e1
  (t2, c2) <- infer g e2
  let c = Set.union c1 c2
  return (TInt, Set.insert (CEq t1 TInt) (Set.insert (CEq t2 TInt) c))
  
infer g (Equal e1 e2) = do
  (t1, c1) <- infer g e1
  (t2, c2) <- infer g e2
  let c = Set.union c1 c2
  return (TBool, Set.insert (CEq t1 t2) c)
  
infer g (ITE e1 e2 e3) = do
  (t1, c1) <- infer g e1
  (t2, c2) <- infer g e2
  (t3, c3) <- infer g e3
  let c = Set.union c1 (Set.union c2 c3)
  return (t2, Set.insert (CEq t1 TBool) (Set.insert (CEq t2 t3) c))
  
infer g (Abs x e) = do
  y <- getFreshTVar
  (t, c) <- infer (Map.insert x y g) e
  return (TArr y t, c)
  
infer g (App e1 e2) = do
  y1 <- getFreshTVar
  y2 <- getFreshTVar
  (t1, c1) <- infer g e1
  (t2, c2) <- infer g e2
  let c = Set.union c1 c2
  return (y2, Set.insert (CEq t1 (TArr y1 y2)) (Set.insert (CEq t2 y1) c))
  
infer g (LetIn varId e1 e2) = do
  y1 <- getFreshTVar
  (t1, c1) <- infer (Map.insert varId y1 g) e1
  (t2, c2) <- infer (Map.insert varId y1 g) e2
  let c = Set.union c1 c2
  return (t2, Set.insert (CEq y1 t1) c)

-- Run type inference on an expression with empty environment
inferExpr :: Expr -> (Type, ConstraintSet)
inferExpr e = evalState (infer Map.empty e) 0

-- Convert constraint set to list for processing
toCstrList :: ConstraintSet -> ConstraintList
toCstrList = Set.toAscList

-- Apply a substitution to a type
applySub :: Substitution -> Type -> Type
applySub ro x = case x of
  TInt      -> TInt
  TBool     -> TBool
  TError    -> TError
  TVar n    -> case Map.lookup (TVar n) ro of
                 Just t  -> t
                 Nothing -> TVar n
  TArr t1 t2 -> TArr (applySub ro t1) (applySub ro t2)

-- Apply a substitution to a constraint
applySubToCstr :: Substitution -> Constraint -> Constraint
applySubToCstr _ CError = CError
applySubToCstr ro (CEq t1 t2) = CEq (applySub ro t1) (applySub ro t2)

-- Apply a substitution to a constraint list
applySubToCstrList :: Substitution -> ConstraintList -> ConstraintList
applySubToCstrList ro cl = map (applySubToCstr ro) cl

-- Compose two substitutions
composeSub :: Substitution -> Substitution -> Substitution
composeSub sub1 sub2 = Map.map (applySub sub1) sub2 `Map.union` sub1

-- Get set of type variables in a type
tvars :: Type -> Set.Set Type
tvars TInt   = Set.empty
tvars TBool  = Set.empty
tvars TError = Set.empty
tvars (TVar n) = Set.singleton (TVar n)
tvars (TArr t1 t2) = Set.union (tvars t1) (tvars t2)

-- Unify constraints to find substitution that satisfies them
unify :: ConstraintList -> Maybe Substitution
unify cs = unify' cs Map.empty
  where
    unify' :: ConstraintList -> Substitution -> Maybe Substitution
    unify' [] sub = Just sub
    unify' (c:cs) sub = case c of
      CError -> Nothing
      CEq t1 t2 -> do
        let t1' = applySub sub t1
            t2' = applySub sub t2
        sub' <- unifyTypes t1' t2'
        let newSub = composeSub sub' sub
        unify' cs newSub

    unifyTypes :: Type -> Type -> Maybe Substitution
    unifyTypes t1 t2
      | t1 == t2 = Just Map.empty
    unifyTypes (TVar n) t = bindVar (TVar n) t
    unifyTypes t (TVar n) = bindVar (TVar n) t 
    unifyTypes (TArr t1 t2) (TArr t3 t4) = do
      sub1 <- unifyTypes t1 t3
      let t2' = applySub sub1 t2
          t4' = applySub sub1 t4
      sub2 <- unifyTypes t2' t4'
      return (composeSub sub2 sub1)
    unifyTypes _ _ = Nothing

    bindVar :: Type -> Type -> Maybe Substitution
    bindVar (TVar n) t
      | t == (TVar n) = Just Map.empty
      | Set.member (TVar n) (tvars t) = Nothing  -- Occurs check
      | otherwise = Just (Map.singleton (TVar n) t)

-- Relabel type variables to make them more readable (t1, t2, etc.)
type RelabelState a = State (Map.Map Int Int) a

relabel :: Type -> Type
relabel t = evalState (go t) Map.empty
  where
    go :: Type -> RelabelState Type
    go TInt = return TInt
    go TBool = return TBool
    go TError = return TError
    go (TVar x) = do 
      m <- get
      case Map.lookup x m of
        Just v -> return (TVar v)
        Nothing -> do 
          let n = 1 + Map.size m
          put (Map.insert x n m)
          return (TVar n)
    go (TArr t1 t2) = do 
      t1' <- go t1
      t2' <- go t2
      return (TArr t1' t2')

-- Main type checking function that combines inference and unification
typeCheck :: Expr -> Either String Type
typeCheck e = 
  case unify (toCstrList cs) of
    Nothing -> Left "Type Error"
    Just sub -> Right (relabel (applySub sub t))
  where 
    (t, cs) = inferExpr e