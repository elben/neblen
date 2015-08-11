module Neblen.Data where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

type NeblenProgram = String

type JSProgram = String

type Name = String

data Value = IntV Int
           | BoolV Bool
           | StringV String
  deriving (Show, Eq)

data Exp = Lit Value
         | List [Exp]
         | Var Name          -- ^ Var "x"
         | Def Exp Exp       -- ^ Def (Var "x") Exp
         | NullaryFun Exp    -- ^ Fun Exp
         | Fun Exp Exp       -- ^ Fun (Var "x") Exp
         | NullaryApp Exp    -- ^ NullaryApp (Fun or Var)
         | UnaryApp Exp Exp  -- ^ UnaryApp (Fun or Var) (Argument value)
         | Let Exp Exp Exp   -- ^ Let (Var "x") (Value of x) Body.
         | If Exp Exp Exp    -- ^ If (Predicate : Bool) (Then clause) (Else clause)
        -- ^ TODO: Convert let to macro that uses function.
  deriving (Show, Eq)

-- | Symbols that can be part of symbol-only identifiers.
validIdSymbols :: String
validIdSymbols = "<>=%^*-+/"

symbolToJsId :: M.Map Char String
symbolToJsId = M.fromList [
  ('<', "lt"),
  ('>', "gt"),
  ('=', "eq"),
  ('%', "percent"),
  ('^', "hat"),
  ('*', "mult"),
  ('-', "minus"),
  ('+', "plus"),
  ('/', "div")]

reservedIds :: S.Set String
reservedIds = S.fromList ["def", "fn", "let"]

standardFuns :: M.Map String JSProgram
standardFuns = M.fromList [
  -- first-or, rest, rest-or
  ("_nbln_firstminusor", "function(list) { return function(or) { if (list.length === 0) { return or; } else { return list[0]; }; }; };"),
  ("_nbln_rest", "function(list) { if (list.length === 0) { return []; } else { return list.slice(1,list.length); }; };"),
  ("_nbln_restminusor", "function(list) { return function(or) { if (list.length === 0) { return or; } else { return list.slice(1,list.length); }; }; };"),

  -- and, or, not, xor
  ("_nbln_and", "function(x) { return function(y) { return x && y; }; };"),
  ("_nbln_or", "function(x) { return function(y) { return x || y; }; };"),
  ("_nbln_not", "function(x) { return !x; };"),
  ("_nbln_xor", "function(x) { return function(y) { return (x && !y) || (y && !x); }; };"),

  -- + - *
  ("_nbln_plus", "function(x) { return function(y) { return x + y; }; };"),
  ("_nbln_minus", "function(x) { return function(y) { return x - y; }; };"),
  ("_nbln_mult", "function(x) { return function(y) { return x * y; }; };")]

