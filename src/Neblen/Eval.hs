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
  e -> e

-- | Evaluates expression.
--
eval :: Exp -> Either TypeError Answer
eval expr =
  liftM (\t -> (eval' M.empty expr, t)) (runWithFreshCounter (checkType expr))

eval' :: EvalEnv -> Exp -> Exp
eval' env expr = case expr of

  Lit{} -> expr

  Var n -> fromMaybe (neblenError expr) (M.lookup n env)

  List es -> List (map (eval' env) es)

  NullaryFun e -> NullaryFun (subst env e)

  Fun (Var n) e -> do Fun (Var n) (subst env e)
  Fun {} -> neblenError expr

  NullaryApp e -> eval' env e

  -- Two cases for application:
  --
  -- ((fn [x] x) 3)
  -- (x y)
  --
  UnaryApp f e -> do
    let e' = eval' env e
    case f of
      Fun (Var n) fn -> do
        let env' = M.insert n e' env
        eval' env' fn
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

  Def {} -> error "TODO"

neblenError :: Exp -> t
neblenError expr = error ("Neblen bug: " ++ toLisp expr ++ " should have been caught by type checker.")
