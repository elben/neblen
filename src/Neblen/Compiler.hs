{-# LANGUAGE OverloadedStrings #-}

module Neblen.Compiler where

import Neblen.Data
import Neblen.Parser
import qualified Data.Map.Strict as M
import qualified Data.List as L

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Neblen.Compiler
--

emptyEnv :: Env
emptyEnv = M.empty

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
-- >>> xformVar "hello-world"
-- "_nbln_hellominusworld"
--
-- TODO: need to make sure vars CAN'T smash other vars with the same name
-- but different scope. I think since we use javascript 'var' this is solved?
xformVar :: Name -> JSProgram
xformVar v = "_nbln_" ++ v'
  where v' = L.intercalate "" (fmap (\c -> M.findWithDefault [c] c symbolToJsId) v)

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


-- | Emit nullary function.
--
-- >>> emitNullaryFunction emptyEnv (Add (Literal (IntV 3)) (Var "x"))
-- "(function () { return (3 + _nbln_x); })"
--
emitNullaryFunction :: Env -> Exp -> JSProgram
emitNullaryFunction env expr = "(function () { return " ++ emitExp env expr ++ "; })"

-- | Emit unary function.
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
-- >>> emitLet emptyEnv (Var "incr") (Function (Var "x") (Add (Literal (IntV 1)) (Var "x"))) (UnaryCall (Var "incr") (Literal (IntV 10)))
-- "(function (_nbln_incr) { return _nbln_incr(10); })((function (_nbln_x) { return (1 + _nbln_x); }))"
--
emitLet :: Env -> Exp -> Exp -> Exp -> JSProgram
emitLet env (Var v) val body = emitUnaryCall env (Function (Var v) body) val
emitLet _ _ _ _ = "Invalid let definition."

-- | Emit nullary function call.
--
-- Env -> Function or function name -> JSProgram
--
emitNullaryCall :: Env -> Exp -> JSProgram
emitNullaryCall _ (Var fn) = xformVar fn ++ "()"
emitNullaryCall env (Function var body) = emitFunction env var body ++ "()"
emitNullaryCall env expr = emitExp env expr ++ "()"

-- | Emit unary function call.
--
-- Env -> Function or function name -> Argument expression -> JSProgram
--
emitUnaryCall :: Env -> Exp -> Exp -> JSProgram
emitUnaryCall env (Var fn) arg = xformVar fn ++ "(" ++ emitExp env arg ++ ")"
emitUnaryCall env (Function var body) arg = emitFunction env var body ++ "(" ++ emitExp env arg ++ ")"
emitUnaryCall env expr arg = emitExp env expr ++ "(" ++ emitExp env arg ++ ")"

-- | Emit vector.
--
-- >>> emitVector emptyEnv []
-- "[]"
--
-- >>> emitVector emptyEnv [Literal (IntV 1), Literal (IntV 3), (Let (Var "incr") (Function (Var "x") (Add (Literal (IntV 1)) (Var "x"))) (UnaryCall (Var "incr") (Literal (IntV 10))))]
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
emitExp env (NullaryFun expr) = emitNullaryFunction env expr
emitExp env (Function var expr) = emitFunction env var expr
emitExp env (UnaryCall fun arg) = emitUnaryCall env fun arg
emitExp env (NullaryCall fun) = emitNullaryCall env fun
emitExp env (Let var val body) = emitLet env var val body

-- | The standard library program.
--
standardLib :: JSProgram
standardLib = M.foldlWithKey (\js fn body -> js ++ "\nvar " ++ fn ++ "=" ++ body) "" standardFunctions ++ "\n\n"

-- | Emit a JavaScript program.
--
emit :: Exp -> JSProgram
emit = emitExp emptyEnv

-- | Compile a line of Neblen to JavaScript.
--
-- >>> compileLine "(foo 1 (fn [x] x) [1 2] (list 4))"
-- "_nbln_foo(1)((function (_nbln_x) { return _nbln_x; }))([1,2])([4])"
--
compileLine :: NeblenProgram -> JSProgram
compileLine p = case parseProgram p of
                  Right expr -> emit expr
                  Left err -> show err

-- | Compile a Neblen program to JavaScript. Includes standard library.
--
compile :: NeblenProgram -> JSProgram
compile p = standardLib ++ compileLine p
