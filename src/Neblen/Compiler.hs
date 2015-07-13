{-# LANGUAGE OverloadedStrings #-}

module Neblen.Compiler where

import qualified Data.Map.Strict as MS
import qualified Data.List as L

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Neblen.Compiler
--

type Env = MS.Map String Value

data Value = IntV Int
           | BoolV Bool
           | StringV String
  deriving (Show, Eq)

data Exp = Literal Value
         | Vector [Exp]
         | Var String
         -- Var "x"
         | Def Exp Exp
         -- Def (Var "x") Exp
         | Add Exp Exp
         | Function Exp Exp
         -- Function (Var "x") Exp
         | Call Exp Exp
         -- Call Function (Argument value)
         | Let Exp Exp Exp
         -- Let (Var "x") (Value of x) Body
  deriving (Show, Eq)

-- eval :: Env -> Exp -> Value
-- eval _ (Literal v) = v
-- eval env (Var var) = case M.lookup var env of
--                        Just v -> v
--                        _      -> error "Variable not in environment"
-- eval env (Def _ expr) = eval env expr

emptyEnv :: Env
emptyEnv = MS.empty

-- Examples
-- eval emptyEnv (IntV 4)
-- eval (M.fromList [("x", IntV 4)]) (Var "x")

type JSProgram = String


-- | Emit Value.
--
-- >>> emitV (IntV (-30))
-- "-30"
--
-- >>> emitV (StringV "Hello")
-- "\"Hello\""
--
emitV :: Value -> JSProgram
emitV (IntV i) = show i
emitV (BoolV True) = "true"
emitV (BoolV False) = "false"
emitV (StringV s) = "\"" ++ s ++ "\""

-- | Emit definition binding.
--
-- >>> emitDef emptyEnv (Var "x") (Add (Literal (IntV 3)) (Literal (IntV 55)))
-- "var x = (3 + 55);\n"
--
-- >>> emitDef emptyEnv (Var "x") (Var "y")
-- "var x = y;\n"
--
emitDef :: Env -> Exp -> Exp -> JSProgram
emitDef env (Var v) expr = "var " ++ v ++ " = " ++ emitExp env expr ++ ";\n"
emitDef _ _ _ = error "Definition has invalid variable name."

-- | Emit function.
--
-- >>> emitFunction emptyEnv (Var "x") (Add (Literal (IntV 3)) (Var "x"))
-- "(function (x) { return (3 + x); })"
--
emitFunction :: Env -> Exp -> Exp -> JSProgram
emitFunction env (Var v) expr = "(function (" ++ v ++ ") { return " ++ emitExp env expr ++ "; })"
emitFunction _ _ _ = error "Invalid function definition."

-- | Emit let binding.
--
-- Env -> Variable name -> Variable value -> Body expression
--
-- Implemented as function call so that the variable is scoped only in the body.
--
-- >>> emitLet emptyEnv (Var "x") (Literal (IntV 55)) (Add (Literal (IntV 3)) (Var "x"))
-- "(function (x) { return (3 + x); })(55)"
--
-- >>> emitLet emptyEnv (Var "incr") (Function (Var "x") (Add (Literal (IntV 1)) (Var "x"))) (Call (Var "incr") (Literal (IntV 10)))
-- "(function (incr) { return incr(10); })((function (x) { return (1 + x); }))"
--
emitLet :: Env -> Exp -> Exp -> Exp -> JSProgram
emitLet env (Var v) val body = emitCall env (Function (Var v) body) val
emitLet _ _ _ _ = "Invalid let definition."

-- | Emit function call.
--
-- Env -> Function or function name -> Argument expression -> JSProgram
--
emitCall :: Env -> Exp -> Exp -> JSProgram
emitCall env (Var fn) expr = fn ++ "(" ++ emitExp env expr ++ ")"
emitCall env (Function var body) expr = emitFunction env var body ++ "(" ++ emitExp env expr ++ ")"
emitCall _ _ _ = error "Invalid function call."

-- | Emit vector.
--
-- >>> emitVector emptyEnv []
-- "[]"
--
-- >>> emitVector emptyEnv [Literal (IntV 1), Literal (IntV 3), (Let (Var "incr") (Function (Var "x") (Add (Literal (IntV 1)) (Var "x"))) (Call (Var "incr") (Literal (IntV 10))))]
-- "[1,3,(function (incr) { return incr(10); })((function (x) { return (1 + x); }))]"
--
emitVector :: Env -> [Exp] -> JSProgram
emitVector _ [] = "[]"
emitVector env exprs = "[" ++ L.intercalate "," (map (emitExp env) exprs) ++ "]"

-- | Emit expression.
--
-- >>> emitExp emptyEnv (Var "x")
-- "x"
--
-- >>> emitExp emptyEnv (Add (Literal (IntV 3)) (Literal (IntV 55)))
-- "(3 + 55)"
emitExp :: Env -> Exp -> JSProgram
emitExp _ (Literal v) = emitV v
emitExp env (Vector v) = emitVector env v
emitExp _ (Var s) = s
emitExp env (Def var expr) = emitDef env var expr
emitExp env (Add left right) = "(" ++ emitExp env left ++ " + " ++ emitExp env right ++ ")"
emitExp env (Function var expr) = emitFunction env var expr
emitExp env (Call fun arg) = emitCall env fun arg
emitExp env (Let var val body) = emitLet env var val body

emit :: Exp -> JSProgram
emit = emitExp emptyEnv
