module AST where

import qualified Data.Map as Map
import qualified Data.Set as Set

-- Variable identifiers
type VarId = String

-- Expression AST
data Expr = CInt Int           -- Integer constant
          | CBool Bool         -- Boolean constant
          | Var VarId          -- Variable reference
          | Plus Expr Expr     -- Addition
          | Minus Expr Expr    -- Subtraction
          | Equal Expr Expr    -- Equality comparison
          | ITE Expr Expr Expr -- If-then-else
          | Abs VarId Expr     -- Lambda abstraction
          | App Expr Expr      -- Function application
          | LetIn VarId Expr Expr  -- Let binding
          deriving (Eq, Ord, Read, Show)

-- Types
data Type = TInt               -- Integer type
          | TBool              -- Boolean type
          | TError             -- Type error indicator
          | TVar Int           -- Type variable for inference
          | TArr Type Type     -- Function type (t1 -> t2)
          deriving (Eq, Ord, Read, Show)

-- Type constraints for inference
data Constraint = CEq Type Type  -- Types must be equal
                | CError         -- Constraint error
                deriving (Eq, Ord, Read, Show)

-- Sets and lists of constraints
type ConstraintSet = Set.Set Constraint
type ConstraintList = [Constraint]

-- Type substitution map
type Substitution = Map.Map Type Type

-- Runtime values
data Value = VInt Int              -- Integer value
           | VBool Bool            -- Boolean value
           | VClosure VarId Expr Environment  -- Function closure with environment
           deriving (Show)

-- Runtime environment mapping variables to values
type Environment = Map.Map VarId Value

-- Pretty printing for types
prettyType :: Type -> String
prettyType TInt = "Int"
prettyType TBool = "Bool"
prettyType TError = "Type Error"
prettyType (TVar n) = "t" ++ show n
prettyType (TArr t1 t2) = 
  case t1 of
    TArr _ _ -> "(" ++ prettyType t1 ++ ") -> " ++ prettyType t2
    _        -> prettyType t1 ++ " -> " ++ prettyType t2