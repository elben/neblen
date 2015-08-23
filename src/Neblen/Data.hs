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
         | Fun [Exp] Exp
         | NullaryApp Exp    -- ^ NullaryApp (Fun or Var)
         | UnaryApp Exp Exp  -- ^ UnaryApp (Fun or Var) (Argument value)
         | Let Exp Exp Exp   -- ^ Let (Var "x") (Value of x) Body.
         | If Exp Exp Exp    -- ^ If (Predicate : Bool) (Then clause) (Else clause)
         | BinOp String Exp Exp
         | DefDataType Name [TName] [Exp] -- ^ DefDataType Name [TName] [DataCtor]
         | DataCtor Name [Type]
         | CtorApp Exp [Exp] -- CtorApp DataCtor [Exp]
  deriving (Show, Eq)

-- Type variable.
type TName = String

-- | Kinds are the "type of types". They are either monotypes or function kinds.
-- Kinds are used to check that a type is well-formed.
--
-- Examples of monotypes: Int, [Int], Maybe a, EitherT e m a.
-- Examples of function kinds: [], Maybe, EitherT.
--
data Kind = Star
          | KFun Kind Kind
          -- | Unresolved kind with free variable counter.
          | KUnknown Int
  deriving (Eq, Ord)

data Type = TUnit
          | TInt
          | TBool
          | TString
          | TFun [Type]
          | TList Type
          | TVar TName

          -- Stuff with kinds:
          | TConst TName Kind
          | TVarK TName Kind -- A type var with a kind
          | TApp Type Type

  deriving (Eq, Ord) -- Ord for Set functions

-- data Type = TVar TName
--           | TConst TName
--           | TApp Type2 Type
--   deriving (Eq, Ord) -- Ord for Set functions

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

instance Show Type where
  show TUnit = "Unit"
  show TInt = "Int"
  show TBool = "Bool"
  show TString = "String"
  show (TFun ts) = "(-> " ++ unwords (map show ts) ++ ")"
  show (TList a) = "[" ++ show a ++ "]"
  show (TVar n) = n
  show (TConst n _) = n
  show (TVarK n _) = n
  show (TApp t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

instance Show Kind where
  show Star = "*"
  show (KFun k1 k2) = "(" ++ show k1 ++ " -> " ++ show k2 ++ ")"
  show (KUnknown i) = "k" ++ show i

