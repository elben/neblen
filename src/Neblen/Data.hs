{-# LANGUAGE OverloadedStrings #-}

module Neblen.Data where

import qualified Data.Map.Strict as MS
import qualified Data.Set as S

type NeblenProgram = String

type JSProgram = String

type Env = MS.Map String Value

data Value = IntV Int
           | BoolV Bool
           | StringV String
  deriving (Show, Eq)

data Exp = Literal Value
         | List [Exp]
         | Vector [Exp]
         | Var String        -- ^ Var "x"
         | Def Exp Exp       -- ^ Def (Var "x") Exp
         | Add Exp Exp
         | NullaryFun Exp    -- ^ Function (Var "x") Exp
         | Function Exp Exp  -- ^ Function (Var "x") Exp
         | UnaryCall Exp Exp -- ^ UnaryCall (Function or Var) (Argument value)
         | NullaryCall Exp   -- ^ NullaryCall (Function or Var)
         | Let Exp Exp Exp   -- ^ Let (Var "x") (Value of x) Body
  deriving (Show, Eq)

-- | Symbols that can be part of symbol-only identifiers.
validIdSymbols :: String
validIdSymbols = "<>=%^*-+/"

symbolToJsId :: MS.Map Char String
symbolToJsId = MS.fromList [
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
reservedIds = S.fromList ["def", "fn"]

