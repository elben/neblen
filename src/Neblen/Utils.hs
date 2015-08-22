
module Neblen.Utils where

import Neblen.Data

-- | Converts the AST to the original Neblen program.
--
-- >>> toLisp (Lit (IntV 0))
-- "0"
--
-- >>> toLisp (Lit (BoolV True))
-- "true"
--
-- >>> toLisp (Lit (StringV "hello"))
-- "\"hello\""
--
-- This won't type-check, but useful for test.
-- >>> toLisp (List [(Lit (StringV "hello")),(Fun [Var "x"] (Var "x")),(UnaryApp (Var "x") (Var "y"))])
-- "[\"hello\" (fn [x] x) (x y)]"
--
-- >>> toLisp (Let (Var "x") (Lit (StringV "hello")) (Var "x"))
-- "(let [x \"hello\"] x)"
--
-- >>> toLisp (If (Var "x") (Var "y") (Var "z"))
-- "(if x y z)"
--
-- >>> toLisp (Def (Var "x") (Var "y"))
-- "(def x y)"
--
-- >>> toLisp (NullaryApp (Var "x"))
-- "(x)"
--
-- >>> toLisp (Fun [] (Var "x"))
-- "(fn [] x)"
--
-- >>> toLisp (DefDataCtor (DataCtor "Foo" [TVar "a",TInt]))
-- "(Foo a Int)"
--
-- >>> toLisp (DefDataCtor (DataCtor "Foo" [TVar "a",TInt]))
-- toLisp (CtorApp "Person" [Lit (StringV "Bob"),Lit (IntV 0)])
-- "(Person \"Bob\" 0)"
--
toLisp :: Exp -> String
toLisp (Lit (IntV v)) = show v
toLisp (Lit (BoolV v)) = if v then "true" else "false"
toLisp (Lit (StringV v)) = show v
toLisp (List []) = "[]"
toLisp (List (a:as)) = "[" ++ toLisp a ++ foldl (\s e -> s ++ " " ++ toLisp e) "" as ++ "]"
toLisp (Var v) = v
toLisp (Def var body) = "(def " ++ toLisp var ++ " " ++ toLisp body ++ ")"
toLisp (Fun vs body) = "(fn [" ++ unwords (map toLisp vs) ++ "] " ++ toLisp body ++ ")"
toLisp (NullaryApp body) = "(" ++ toLisp body ++ ")"
toLisp (UnaryApp fn body) = "(" ++ toLisp fn ++ " " ++ toLisp body ++ ")"
toLisp (Let v e body) = "(let [" ++ toLisp v ++ " " ++ toLisp e ++ "] " ++ toLisp body ++ ")"
toLisp (If p t e) = "(if " ++ toLisp p ++ " " ++ toLisp t ++ " " ++ toLisp e ++ ")"
toLisp (BinOp f a b) = "(" ++ f ++ " " ++ toLisp a ++ " " ++ toLisp b ++ ")"
-- toLisp (DefDataType {}) = "NOT IMPLEMENTED"
-- toLisp (DataCtor {}) = "NOT IMPLEMENTED"
-- toLisp (CtorApp n exprs) = "(" ++ n ++ " " ++ unwords (map toLisp exprs) ++ ")"

-- | Pretty-format a Neblen program.
pretty :: String -> String
pretty s = s
