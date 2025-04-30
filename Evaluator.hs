module Evaluator where

import AST
import qualified Data.Map as Map

-- Empty environment
emptyEnv :: Environment
emptyEnv = Map.empty

-- Evaluate an expression in the given environment
eval :: Environment -> Expr -> Either String Value
eval env (CInt n) = Right $ VInt n
eval env (CBool b) = Right $ VBool b

eval env (Var id) = 
  case Map.lookup id env of
    Just val -> Right val
    Nothing -> Left $ "Runtime error: Unbound variable '" ++ id ++ "'"

eval env (Plus e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VInt (n1 + n2)
    _ -> Left "Runtime error: Addition requires integer operands"

eval env (Minus e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VInt (n1 - n2)
    _ -> Left "Runtime error: Subtraction requires integer operands"

eval env (Equal e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VBool (n1 == n2)
    (VBool b1, VBool b2) -> Right $ VBool (b1 == b2)
    _ -> Left "Runtime error: Equality comparison requires same type operands"

eval env (ITE cond thenExpr elseExpr) = do
  condVal <- eval env cond
  case condVal of
    VBool True -> eval env thenExpr
    VBool False -> eval env elseExpr
    _ -> Left "Runtime error: Condition must be a boolean"

eval env (Abs varId body) = 
  -- Create a closure that captures the current environment
  Right $ VClosure varId body env

eval env (App fun arg) = do
  funVal <- eval env fun
  argVal <- eval env arg
  case funVal of
    VClosure varId body closureEnv -> 
      -- Apply the function by evaluating its body in an extended environment
      eval (Map.insert varId argVal closureEnv) body
    _ -> Left "Runtime error: Function application requires a function"

eval env (LetIn varId bindExpr bodyExpr) = do
  bindVal <- eval env bindExpr
  -- Evaluate the body with the binding in the environment
  eval (Map.insert varId bindVal env) bodyExpr

-- Pretty printing for values
showValue :: Value -> String
showValue (VInt n) = show n
showValue (VBool b) = if b then "true" else "false"
showValue (VClosure _ _ _) = "<function>"

-- Evaluate an expression with an empty environment
evaluate :: Expr -> Either String String
evaluate expr = do
  value <- eval emptyEnv expr
  return $ showValue value