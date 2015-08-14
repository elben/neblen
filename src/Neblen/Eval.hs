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

-- | Substitute variables in expression, but don't apply.
--
subst :: EvalEnv -> Exp -> Exp
subst env expr = case expr of
  Lit {} -> expr
  List es -> List (map (subst env) es)
  Var n -> fromMaybe (Var n) (M.lookup n env)
  Fun vs body -> Fun vs (subst env body)
  UnaryApp f e -> UnaryApp (subst env f) (subst env e)
  BinOp f a b -> BinOp f (subst env a) (subst env b)
  e -> e

defaultEnv :: M.Map Name Exp
defaultEnv = M.fromList [
  ("+", Fun [Var "a",Var "b"] (BinOp "+" (Var "a") (Var "b")))
 ,("-", Fun [Var "a",Var "b"] (BinOp "-" (Var "a") (Var "b")))
 ,("*", Fun [Var "a",Var "b"] (BinOp "*" (Var "a") (Var "b")))

 ,("and", Fun [Var "a",Var "b"] (BinOp "and" (Var "a") (Var "b")))
 ,("or", Fun [Var "a",Var "b"] (BinOp "or" (Var "a") (Var "b")))
 ,("xor", Fun [Var "a",Var "b"] (BinOp "xor" (Var "a") (Var "b")))
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

  Fun vs e -> Fun vs (subst env e)

  -- NullaryApp should only be called on nullary functions.
  NullaryApp (Fun [] body) -> eval' env body
  NullaryApp _ -> neblenError expr

  -- Two cases for function application:
  --
  -- - If `f` is a function, apply the expression into the function.
  -- - Otherwise, re-try the application after eval-ing `f`.
  --
  UnaryApp f e -> do
    let e' = eval' env e
    case f of
      -- Only one arg left, so do the apply.
      Fun [Var n] fn -> do
        let env' = M.insert n e' env
        eval' env' fn

      -- Curry; apply only one level.
      Fun (Var n:vs) fn -> do
        let env' = M.insert n e' env
        eval' env' (Fun vs fn)

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
