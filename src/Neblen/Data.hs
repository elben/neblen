{-# LANGUAGE OverloadedStrings #-}

module Neblen.Data where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

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

-- Mapping of variables to its type.
type TEnv = M.Map Name Type

-- Type variable.
type TName = String

-- Unification tenvironment. Mapping of type variables to its type.
type UEnv = M.Map TName Type

-- Monad transformer stack for TypeCheck:
--
--   State (fresh variable counter)
--     ExceptT (TypeError)
--       (TEnv, UEnv, Type)
--
type TypeCheck = ExceptT TypeError (State FreshCounter) (TEnv, UEnv, Type)

-- TODO extract literal types to TLit, similar to "Literal" for expressions?
data Type = TInt
          | TBool
          | TString
          | TFun Type Type
          | TList Type
          | TVec Type
          | TVar TName -- Type variable.
  deriving (Eq)

instance Show Type where
  show TInt = "Int"
  show TBool = "Bool"
  show TString = "String"
  show (TFun a r) = "(-> " ++ show a ++ " " ++ show r ++ ")"
  show (TList a) = "(Vector " ++ show a ++ ")"
  show (TVec a) = "[" ++ show a ++ "]"
  show (TVar n) = n

data TypeError = Mismatch Type Type
               | UnboundVariable Name
               | InfiniteType Type Type -- InfiteType TVar Type
               | GenericTypeError (Maybe String)


emptyGenericTypeError :: TypeError
emptyGenericTypeError = GenericTypeError Nothing

genericTypeError :: String -> TypeError
genericTypeError msg = GenericTypeError (Just msg)

instance Show TypeError where
  show (Mismatch t1 t2) = "type mismatch: expecting " ++ show t1 ++ " but got " ++ show t2
  show (UnboundVariable n) = "unbound variable " ++ n
  show (InfiniteType tvar t) = "cannot resolve infintie type " ++ show tvar ++ " in " ++ show t
  show (GenericTypeError (Just msg)) = "type error: " ++ msg
  show (GenericTypeError Nothing) = "type error"

newtype FreshCounter = FreshCounter { getFreshCounter :: Int }

initFreshCounter :: FreshCounter
initFreshCounter = FreshCounter { getFreshCounter = 0 }

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']


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

