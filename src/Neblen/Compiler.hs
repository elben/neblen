{-# LANGUAGE OverloadedStrings #-}

module Neblen.Compiler where

import Neblen.Data
import Neblen.Parser
import qualified Data.Map.Strict as MS
import qualified Data.List as L

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Neblen.Compiler
--

emptyEnv :: Env
emptyEnv = MS.empty

-- | Emit Value.
--
-- >>> emitValue (IntV (-30))
-- "-30"
--
-- >>> emitValue (StringV "Hello")
-- "\"Hello\""
--
emitValue :: Value -> JSProgram
emitValue (IntV i) = show i
emitValue (BoolV True) = "true"
emitValue (BoolV False) = "false"
emitValue (StringV s) = "\"" ++ s ++ "\""

-- https://mathiasbynens.be/notes/javascript-identifiers
-- toJsVar :: String -> JSProgram
-- toJsVar

-- | Emit variable identifier.
--
-- >>> xformVar "+++"
-- "_nbln_plusplusplus"
--
-- >>> xformVar "hello"
-- "_nbln_hello"
--
xformVar :: String -> JSProgram
xformVar v = "_nbln_" ++ v'
  where v' = L.intercalate "" (fmap (\c -> MS.findWithDefault [c] c symbolToJsId) v)

-- | Emit definition binding.
--
-- >>> emitDef emptyEnv (Var "x") (Add (Literal (IntV 3)) (Literal (IntV 55)))
-- "var _nbln_x = (3 + 55);\n"
--
-- >>> emitDef emptyEnv (Var "x") (Var "y")
-- "var _nbln_x = _nbln_y;\n"
--
emitDef :: Env -> Exp -> Exp -> JSProgram
emitDef env (Var v) expr = "var " ++ xformVar v ++ " = " ++ emitExp env expr ++ ";\n"
emitDef _ _ _ = error "Definition has invalid variable name."

-- | Emit function.
--
-- >>> emitFunction emptyEnv (Var "x") (Add (Literal (IntV 3)) (Var "x"))
-- "(function (_nbln_x) { return (3 + _nbln_x); })"
--
emitFunction :: Env -> Exp -> Exp -> JSProgram
emitFunction env (Var v) expr = "(function (" ++ xformVar v ++ ") { return " ++ emitExp env expr ++ "; })"
emitFunction _ _ _ = error "Invalid function definition."

-- | Emit let binding.
--
-- Env -> Variable name -> Variable value -> Body expression
--
-- Implemented as function call so that the variable is scoped only in the body.
--
-- >>> emitLet emptyEnv (Var "x") (Literal (IntV 55)) (Add (Literal (IntV 3)) (Var "x"))
-- "(function (_nbln_x) { return (3 + _nbln_x); })(55)"
--
-- >>> emitLet emptyEnv (Var "incr") (Function (Var "x") (Add (Literal (IntV 1)) (Var "x"))) (Call (Var "incr") (Literal (IntV 10)))
-- "(function (_nbln_incr) { return _nbln_incr(10); })((function (_nbln_x) { return (1 + _nbln_x); }))"
--
emitLet :: Env -> Exp -> Exp -> Exp -> JSProgram
emitLet env (Var v) val body = emitCall env (Function (Var v) body) val
emitLet _ _ _ _ = "Invalid let definition."

-- | Emit function call.
--
-- Env -> Function or function name -> Argument expression -> JSProgram
--
emitCall :: Env -> Exp -> Exp -> JSProgram
emitCall env (Var fn) expr = xformVar fn ++ "(" ++ emitExp env expr ++ ")"
emitCall env (Function var body) expr = emitFunction env var body ++ "(" ++ emitExp env expr ++ ")"
emitCall _ _ _ = error "Invalid function call."

-- | Emit vector.
--
-- >>> emitVector emptyEnv []
-- "[]"
--
-- >>> emitVector emptyEnv [Literal (IntV 1), Literal (IntV 3), (Let (Var "incr") (Function (Var "x") (Add (Literal (IntV 1)) (Var "x"))) (Call (Var "incr") (Literal (IntV 10))))]
-- "[1,3,(function (_nbln_incr) { return _nbln_incr(10); })((function (_nbln_x) { return (1 + _nbln_x); }))]"
--
emitVector :: Env -> [Exp] -> JSProgram
emitVector _ [] = "[]"
emitVector env exprs = "[" ++ L.intercalate "," (map (emitExp env) exprs) ++ "]"

-- | Emit expression.
--
-- >>> emitExp emptyEnv (Var "x")
-- "_nbln_x"
--
-- >>> emitExp emptyEnv (Add (Literal (IntV 3)) (Literal (IntV 55)))
-- "(3 + 55)"
--
emitExp :: Env -> Exp -> JSProgram
emitExp _ (Literal v) = emitValue v
emitExp env (Vector v) = emitVector env v
emitExp env (List v) = emitVector env v
emitExp _ (Var s) = xformVar s
emitExp env (Def var expr) = emitDef env var expr
emitExp env (Add left right) = "(" ++ emitExp env left ++ " + " ++ emitExp env right ++ ")"
emitExp env (Function var expr) = emitFunction env var expr
emitExp env (Call fun arg) = emitCall env fun arg
emitExp env (Let var val body) = emitLet env var val body

-- | Emit a JavaScript program.
--
emit :: Exp -> JSProgram
emit = emitExp emptyEnv

-- | Compile a Nelbn program to JavaScript.
--
-- >>> compile "(+ 1 2 3)"
-- "[_nbln_plus,1,2,3]"
compile :: NeblenProgram -> JSProgram
compile p = case parseProgram p of
              Right expr -> emit expr
              Left err -> show err
