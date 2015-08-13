module Neblen.Eval where

import Neblen.Data
import Neblen.Utils
import Neblen.TypeChecker
import Neblen.Parser

import qualified Data.Map.Strict as M
import Control.Monad
import Data.Maybe (fromMaybe)

type EvalEnv = M.Map Name Exp

type Answer = (Exp, Type)

data EvalError = UnboundedVariable Name
               | GenericError String

-- | Evaluates Neblen program.
parseAndEval :: NeblenProgram -> Either String Answer
parseAndEval p =
  case parseProgram p of
    Left err -> Left $ show err
    Right expr ->
      case eval expr of
        Left err -> Left $ show err
        Right answer -> Right answer

-- TODO is this what we need? What is the difference between eval and
-- substitution???
--
-- Some diff:
--
-- - Function applications don't get applied, just substituted.
-- - In a (Fun v e), v is also substituted.
--
subst :: EvalEnv -> Exp -> Exp
subst env expr = case expr of
  Lit {} -> expr
  List es -> List (map (subst env) es)
  Var n -> fromMaybe (Var n) (M.lookup n env)
  NullaryFun e -> NullaryFun (subst env e)
  Fun v e -> Fun v (subst env e)
  UnaryApp f e -> UnaryApp (subst env f) (subst env e)
  BinOp f a b -> BinOp f (subst env a) (subst env b)
  e -> e

defaultEnv :: M.Map Name Exp
defaultEnv = M.fromList [
  ("+", MultiFun [Var "a",Var "b"] (BinOp "+" (Var "a") (Var "b")))
 ,("-", MultiFun [Var "a",Var "b"] (BinOp "-" (Var "a") (Var "b")))
 ,("*", MultiFun [Var "a",Var "b"] (BinOp "*" (Var "a") (Var "b")))

 ,("and", MultiFun [Var "a",Var "b"] (BinOp "and" (Var "a") (Var "b")))
 ,("or", MultiFun [Var "a",Var "b"] (BinOp "or" (Var "a") (Var "b")))
 ,("xor", MultiFun [Var "a",Var "b"] (BinOp "xor" (Var "a") (Var "b")))
 ]

-- | Evaluates expression.
--
eval :: Exp -> Either TypeError Answer
eval expr =
  liftM (\t -> (eval' defaultEnv expr, t)) (runWithFreshCounter (checkType expr))

eval' :: EvalEnv -> Exp -> Exp
eval' env expr = case expr of

  Lit{} -> expr

  Var n -> fromMaybe (neblenError expr) (M.lookup n env)

  List es -> List (map (eval' env) es)

  NullaryFun e -> NullaryFun (subst env e)

  Fun (Var n) e -> Fun (Var n) (subst env e)
  Fun {} -> neblenError expr

  MultiFun vs e -> MultiFun vs (subst env e)

  NullaryApp e -> eval' env e

  -- Two cases for function application:
  --
  -- - If `f` is a function, apply the expression into the function.
  -- - Otherwise, re-try the application after eval-ing `f`.
  --
  UnaryApp f e -> do
    let e' = eval' env e
    case f of
      -- Fun (Var n) fn -> do
      --   let env' = M.insert n e' env
      --   eval' env' fn

      -- Only one arg left, so do the apply.
      MultiFun [Var n] fn -> do
        let env' = M.insert n e' env
        eval' env' fn

      -- Curry; apply only one level.
      MultiFun (Var n:vs) fn -> do
        let env' = M.insert n e' env
        eval' env' (MultiFun vs fn)

      other -> do
        let f' = eval' env other
        eval' env (UnaryApp f' e)

  Let (Var n) val body -> do
    let val' = eval' env val
    let env' = M.insert n val' env
    eval' env' body
  Let{} -> neblenError expr

  If p t e -> do
    let p' = eval' env p
    case p' of
      Lit (BoolV True) -> eval' env t
      Lit (BoolV False) -> eval' env e
      _ -> neblenError expr

  BinOp fn a b -> case fn of
    "+" -> Lit (IntV (extractInt (eval' env a) + extractInt (eval' env b)))
    "-" -> Lit (IntV (extractInt (eval' env a) - extractInt (eval' env b)))
    "*" -> Lit (IntV (extractInt (eval' env a) * extractInt (eval' env b)))

    "and" -> Lit (BoolV (extractBool (eval' env a) && extractBool (eval' env b)))
    "or" -> Lit (BoolV (extractBool (eval' env a) || extractBool (eval' env b)))
    "xor" ->
      let a' = extractBool (eval' env a)
          b' = extractBool (eval' env b)
          c = (a' && not b') || (not a' && b')
      in Lit (BoolV c)
    _ -> error "Invalid BinOp."

  Def {} -> error "TODO"

extractInt :: Exp -> Int
extractInt (Lit (IntV v)) = v
extractInt _ = error "not an int"

extractBool :: Exp -> Bool
extractBool (Lit (BoolV v)) = v
extractBool _ = error "not a bool"

extractString :: Exp -> String
extractString (Lit (StringV v)) = v
extractString _ = error "not a string"

neblenError :: Exp -> t
neblenError expr = error ("Neblen bug: " ++ toLisp expr ++ " should have been caught by type checker.")
