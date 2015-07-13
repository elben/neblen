{-# LANGUAGE OverloadedStrings #-}

module Neblen.Compiler where

import qualified Data.Map.Strict as MS
-- import qualified Data.Map as M

-- 3
-- "Hello"
-- x
--
-- (def x 3)
-- Def (Var "x") Exp

type Env = MS.Map String Value

data Value = IntV Int
           | BoolV Bool
           | StringV String
  deriving (Show, Eq)

data Exp = Literal Value
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

-- emptyEnv :: Env
-- emptyEnv = MS.empty

-- Examples
-- eval emptyEnv (IntV 4)
-- eval (M.fromList [("x", IntV 4)]) (Var "x")

type JSProgram = String


emitV :: Value -> JSProgram
emitV (IntV i) = show i
emitV (BoolV True) = "true"
emitV (BoolV False) = "false"
emitV (StringV s) = "\"" ++ s ++ "\""

emitDef :: Exp -> Exp -> JSProgram
emitDef (Var v) expr = "var " ++ v ++ " = " ++ emitExp expr ++ ";\n"
emitDef _ _ = error "Definition has invalid variable name"

emitFunction :: Exp -> Exp -> JSProgram
emitFunction (Var v) expr = "(function (" ++ v ++ ") { return " ++ emitExp expr ++ " })"
emitFunction _ _ = error "Invalid function definition."

emitLet :: Exp -> Exp -> Exp -> JSProgram
emitLet (Var v) val body = emitDef (Var v) val ++ emitExp body
emitLet _ _ _ = "Invalid let definition."

emitCall :: Exp -> Exp -> JSProgram
emitCall (Var fn) expr = fn ++ "(" ++ emitExp expr ++ ")"
emitCall (Function var fnExpr) expr = (emitFunction var fnExpr) ++ "(" ++ emitExp expr ++ ")"
emitCall _ _ = error "Invalid function call."

emitExp :: Exp -> JSProgram
emitExp (Literal v) = emitV v
emitExp (Var s) = s
emitExp (Def var expr) = emitDef var expr
emitExp (Add left right) = "(" ++ emitExp left ++ " + " ++ emitExp right ++ ")"
emitExp (Function var expr) = emitFunction var expr
emitExp (Call fun arg) = emitCall fun arg
emitExp (Let var val body) = emitLet var val body

-- Examples
--
-- emitExp (Var "x")
--
-- emitExp (Add (Literal (IntV 3)) (Literal (IntV 55)))
-- emitExp (Def (Var "x") (Add (Literal (IntV 3)) (Literal (IntV 55))))
--
-- emitExp (Function (Var "x") (Add (Literal (IntV 3)) (Var "x")))
--
-- emitExp (Let (Var "x") (Literal (IntV 55)) (Add (Literal (IntV 3)) (Var "x")))
--
-- emitExp (Let (Var "incr") (Function (Var "x") (Add (Literal (IntV 1)) (Var "x"))) (Call (Var "incr") (Literal (IntV 10))))
--

