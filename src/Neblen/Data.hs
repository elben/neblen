{-# LANGUAGE OverloadedStrings #-}

module Neblen.Data where

import qualified Data.Map.Strict as MS

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
         | Var String       -- ^ Var "x"
         | Def Exp Exp      -- ^ Def (Var "x") Exp
         | Add Exp Exp
         | Function Exp Exp -- ^ Function (Var "x") Exp
         | Call Exp Exp     -- ^ Call Function (Argument value)
         | Let Exp Exp Exp  -- ^ Let (Var "x") (Value of x) Body
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

