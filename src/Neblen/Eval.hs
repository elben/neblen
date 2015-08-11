module Neblen.Eval where

import Neblen.Data
import Neblen.Utils
import Neblen.TypeChecker
import Neblen.Parser

import qualified Data.Map.Strict as M
import Control.Monad
import Data.Maybe (fromMaybe)

type EvalEnv = M.Map Name Exp

data EvalError = UnboundedVariable Name
               | GenericError String

-- | Evaluates Neblen program.
evalS :: NeblenProgram -> NeblenProgram
evalS p =
  case parseProgram p of
    Left err -> show err
    Right expr ->
      case eval expr of
        Left err -> show err
        Right expr' -> toLisp expr'

-- | Evaluates expression.
--
eval :: Exp -> Either TypeError Exp
eval expr =
  liftM (\_ -> eval' M.empty expr) (runWithFreshCounter (checkType expr))

eval' :: EvalEnv -> Exp -> Exp
eval' env expr = case expr of

  Lit{} -> expr

  Var n -> fromMaybe (neblenError expr) (M.lookup n env)

  List es -> List (map (eval' env) es)

  NullaryFun {} -> expr

  Fun {} -> expr

  NullaryApp e -> eval' env e

  -- Two types of application:
  --
  -- ((fn [x] x) 3)
  -- (let [f (fn [x] x)] (f 3))
  --
  UnaryApp f e -> do
    let e' = eval' env e
    case f of
      Fun (Var n) fn -> do
        let env' = M.insert n e' env
        eval' env' fn
      Var n ->
        case M.lookup n env of
          Just fn -> eval' env (UnaryApp fn e)
          Nothing -> neblenError expr
      _ -> neblenError expr

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
  Add {} -> error "TODO"

neblenError :: Exp -> t
neblenError expr = error ("Neblen bug: " ++ toLisp expr ++ " should have been caught by type checker.")
