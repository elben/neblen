{-# LANGUAGE OverloadedStrings #-}

module Neblen.Data where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

type NeblenProgram = String

type JSProgram = String

type Env = M.Map String Value

type Name = String

data Value = IntV Int
           | BoolV Bool
           | StringV String
  deriving (Show, Eq)

data Exp = Literal Value
         | List [Exp]
         | Vector [Exp]
         | Var Name          -- ^ Var "x"
         | Def Exp Exp       -- ^ Def (Var "x") Exp
         | Add Exp Exp
         | NullaryFun Exp    -- ^ Function Exp
         | Function Exp Exp  -- ^ Function (Var "x") Exp
         | UnaryCall Exp Exp -- ^ UnaryCall (Function or Var) (Argument value)
         | NullaryCall Exp   -- ^ NullaryCall (Function or Var)
         | Let Exp Exp Exp   -- ^ Let (Var "x") (Value of x) Body.
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

standardFunctions :: M.Map String JSProgram
standardFunctions = M.fromList [
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

