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

type Env = M.Map String Value

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
-- >>> emitDef emptyEnv (Var "x") (UnaryApp (Var "incr") (Lit (IntV 10)))
-- "var _nbln_x = _nbln_incr(10);\n"
--
-- >>> emitDef emptyEnv (Var "x") (Var "y")
-- "var _nbln_x = _nbln_y;\n"
--
emitDef :: Env -> Exp -> Exp -> JSProgram
emitDef env (Var v) expr = "var " ++ xformVar v ++ " = " ++ emitExp env expr ++ ";\n"
emitDef _ _ _ = error "Definition has invalid variable name."


-- | Emit nullary function.
--
-- >>> emitNullaryFun emptyEnv (UnaryApp (Var "incr") (Var "x"))
-- "(function () { return _nbln_incr(_nbln_x); })"
--
emitNullaryFun :: Env -> Exp -> JSProgram
emitNullaryFun env expr = "(function () { return " ++ emitExp env expr ++ "; })"

-- | Emit unary function.
--
-- >>> emitFun emptyEnv (Var "x") (UnaryApp (Var "incr") (Var "x"))
-- "(function (_nbln_x) { return _nbln_incr(_nbln_x); })"
--
emitFun :: Env -> Exp -> Exp -> JSProgram
emitFun env (Var v) expr = "(function (" ++ xformVar v ++ ") { return " ++ emitExp env expr ++ "; })"
emitFun _ _ _ = error "Invalid function definition."

-- | Emit let binding.
--
-- Env -> Variable name -> Variable value -> Body expression
--
-- Implemented as function call so that the variable is scoped only in the body.
--
-- >>> emitLet emptyEnv (Var "x") (Lit (IntV 55)) (Var "x")
-- "(function (_nbln_x) { return _nbln_x; })(55)"
--
-- >>> emitLet emptyEnv (Var "incr") (Fun (Var "x") (Var "x")) (UnaryApp (Var "incr") (Lit (IntV 10)))
-- "(function (_nbln_incr) { return _nbln_incr(10); })((function (_nbln_x) { return _nbln_x; }))"
--
emitLet :: Env -> Exp -> Exp -> Exp -> JSProgram
-- emitLet env (Var v) val body = emitUnaryApp env (Fun (Var v) body) val
emitLet _ _ _ _ = "Invalid let definition."

-- | Emit nullary function call.
--
-- Env -> Fun or function name -> JSProgram
--
emitNullaryApp :: Env -> Exp -> JSProgram
emitNullaryApp _ (Var fn) = xformVar fn ++ "()"
-- emitNullaryApp env (Fun var body) = emitFun env var body ++ "()"
emitNullaryApp env expr = emitExp env expr ++ "()"

-- | Emit unary function call.
--
-- Env -> Fun or function name -> Argument expression -> JSProgram
--
emitUnaryApp :: Env -> Exp -> Exp -> JSProgram
emitUnaryApp env (Var fn) arg = xformVar fn ++ "(" ++ emitExp env arg ++ ")"
-- emitUnaryApp env (Fun var body) arg = emitFun env var body ++ "(" ++ emitExp env arg ++ ")"
emitUnaryApp env expr arg = emitExp env expr ++ "(" ++ emitExp env arg ++ ")"

-- | Emit vector.
--
-- >>> emitVector emptyEnv []
-- "[]"
--
-- >>> emitVector emptyEnv [Lit (IntV 1), Lit (IntV 3), (Let (Var "incr") (Fun (Var "x") (Var "x")) (UnaryApp (Var "incr") (Lit (IntV 10))))]
-- "[1,3,(function (_nbln_incr) { return _nbln_incr(10); })((function (_nbln_x) { return _nbln_x; }))]"
--
emitVector :: Env -> [Exp] -> JSProgram
emitVector _ [] = "[]"
emitVector env exprs = "[" ++ L.intercalate "," (map (emitExp env) exprs) ++ "]"

-- | Emit if.
--
-- >>> emitIf emptyEnv (If (Lit (BoolV True)) (Lit (IntV 1)) (Lit (IntV 2)))
-- "if (true) { return 1; } else { return 2; }"
--
emitIf :: Env -> Exp -> JSProgram
emitIf env (If p t e) = "if (" ++ emitExp env p ++ ") { return " ++ emitExp env t ++ "; } else { return " ++ emitExp env e ++ "; }"
emitIf _ _ = error "Invalid if statement."

-- | Emit expression.
--
-- >>> emitExp emptyEnv (Var "x")
-- "_nbln_x"
--
emitExp :: Env -> Exp -> JSProgram
emitExp _ (Lit v) = emitValue v
emitExp env (List v) = emitVector env v
emitExp _ (Var s) = xformVar s
-- emitExp env (NullaryFun expr) = emitNullaryFun env expr
-- emitExp env (Fun var expr) = emitFun env var expr
emitExp env (UnaryApp fun arg) = emitUnaryApp env fun arg
emitExp env (NullaryApp fun) = emitNullaryApp env fun
emitExp env (Let var val body) = emitLet env var val body
emitExp env e@(If {}) = emitIf env e

-- | The standard library program.
--
standardLib :: JSProgram
standardLib = M.foldlWithKey (\js fn body -> js ++ "\nvar " ++ fn ++ "=" ++ body) "" standardFuns ++ "\n\n"

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
