{-# LANGUAGE OverloadedStrings #-}

module Neblen.TypeChecker where

import Neblen.Data
import qualified Data.Map.Strict as M

type TEnv = M.Map Name Type

type TName = String

data Type = TInt
          | TBool
          | TString
          | TFun Type Type
          | TList Type
          | TVec Type
          | TFree
          | TVar TName -- Type variable.
  deriving (Show, Eq)

data TypeError = Mismatch Type Type
  deriving (Show)

-- | Checks if type is free.
--
-- >>> isFree (TVar "x")
-- True
--
-- >>> isFree TInt
-- False
--
isFree :: Type -> Bool
isFree (TVar _) = True
isFree _ = False

-- | Checks if type is bound.
--
-- >>> isBound TInt
-- True
--
-- >>> isBound (TVar "x")
-- False
--
isBound :: Type -> Bool
isBound = not . isFree

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Map.Strict as M

emptyTEnv :: TEnv
emptyTEnv = M.empty

lookupEnv :: TEnv -> Name -> Maybe Type
lookupEnv env name = M.lookup name env

insertEnv :: TEnv -> Name -> Type -> TEnv
insertEnv env name t = M.insert name t env

-- | Type check expression.
--
-- -- TODO: Convert to monad transformer so that TEnv is passed along
checkExp :: TEnv -> Exp -> (TEnv, Type)

-- >>> checkExp emptyTEnv (Literal (IntV 0))
-- (fromList [],TInt)
--
checkExp env (Literal (IntV _)) = (env, TInt)

-- >>> checkExp emptyTEnv (Literal (BoolV 0))
-- (fromList [],TBool)
--
checkExp env (Literal (BoolV _)) = (env, TBool)


-- >>> checkExp emptyTEnv (Literal (StringV 0))
-- (fromList [],TString)
--
checkExp env (Literal (StringV _)) = (env, TString)

-- >>> checkExp (M.fromList [("x",TInt)]) (Var "x")
-- (fromList [("x",TInt)],TInt)
--
-- >>> checkExp emptyTEnv (Var "x")
-- TODO should error
--
checkExp env (Var v) =
  case lookupEnv env v of
    Just t  -> (env, t)
    Nothing -> error "unbounded variable"

-- >>> checkExp emptyTEnv (Let (Var "x") (Literal (IntV 0)) (Var "x"))
-- (fromList [],TInt)
--
-- >>> checkExp emptyTEnv (Let (Var "x") (Literal (IntV 0)) (Literal (BoolV True)))
-- (fromList [],TBool)
--
-- >>> checkExp (M.fromList [("b",TBool)]) (Let (Var "x") (Literal (IntV 0)) (Var "b"))
-- (fromList [("b",TBool)],TBool)
--
checkExp env (Let (Var v) val body) =
  let (env', valT) = checkExp env val
      env'' = insertEnv env' v valT
      (_, valB) = checkExp env'' body
  -- Return original env because 'v' is no longer in scope.
  in (env, valB)

-- >>> checkExp emptyTEnv (NullaryFun (Var "x"))
-- TODO: should error since "x" is not in the env.
--
-- >>> checkExp emptyTEnv (NullaryFun (Literal (IntV 0)))
-- (fromList [],TInt)
--
checkExp env (NullaryFun body) = checkExp env body

-- >>> checkExp emptyTEnv (Function (Var "x") (Var "x"))
-- (fromList [],TFun (TVar "x") (TVar "x"))
--
-- >>> checkExp emptyTEnv (Function (Var "x") (Literal (IntV 0)))
-- (fromList [],TFun (TVar "x") TInt)
--
-- >>> checkExp emptyTEnv (Function (Var "x") (Function (Var "y") (Literal (IntV 0))))
-- (fromList [],TFun (TVar "x") (TFun (TVar "y") TInt))
--
-- Verify that argument variable doesn't override outside scope:
--   Environment: x : Bool
--   (fn [x] (fn [y] 0))
--
-- >>> checkExp (M.fromList [("x",TBool)]) (Function (Var "x") (Function (Var "y") (Literal (IntV 0))))
-- (fromList [("x",TBool)],TFun (TVar "x") (TFun (TVar "y") TInt))
--
-- Below is: (fn [x] (fn [y] x))
-- >>> checkExp emptyTEnv (Function (Var "x") (Function (Var "y") (Var "x")))
-- (fromList [],TFun (TVar "x") (TFun (TVar "y") (TVar "x")))
--
checkExp env (Function (Var v) body) =
  let env' = insertEnv env v (TVar v)        -- Set argument as free
      (_, bodyT) = checkExp env' body -- Check body
  in (env, TFun (TVar v) bodyT)

-- >>> checkExp (M.fromList [("x",TBool)]) (NullaryCall (Var "x"))
-- (fromList [("x",TBool)],TBool)
--
-- >>> checkExp emptyTEnv (NullaryCall (NullaryFun (Literal (BoolV True))))
-- (fromList [],TBool)
--
-- >>> checkExp emptyTEnv (NullaryCall (Var "x"))
-- error!
--
checkExp env (NullaryCall (Var v)) = checkExp env (Var v)
checkExp env (NullaryCall (NullaryFun body)) = checkExp env (NullaryFun body)

-- >>> checkExp (M.fromList [("x",TFun TInt TBool)]) (UnaryCall (Var "x") (Literal (IntV 0)))
-- (fromList [("x",TFun TInt TBool)],TBool)
--
-- Below is the case:
--   Environment:
--     f : (-> Bool Bool)
--   (let [x true] (f x))
--
-- >>> checkExp (M.fromList [("f",TFun TBool TBool)]) (Let (Var "x") (Literal (BoolV True)) (UnaryCall (Var "f") (Var "x")))
-- (fromList [("f",TFun TBool TBool)],TBool)
--
-- Below is the case:
--   Environment:
--     f : (-> Int Bool)
--   ((fn [x] x) f)
--
-- >>> checkExp (M.fromList [("f",TFun TInt TBool)]) (UnaryCall (Function (Var "x") (Var "x")) (Var "f"))
-- (fromList [("f",TFun TInt TBool)],TFun TInt TBool)
--
-- Below is: (let [x (fn [y] y)] (x 3))
-- >>> checkExp emptyTEnv (Let (Var "x") (Function (Var "y") (Var "y")) (UnaryCall (Var "x") (Literal (IntV 3))))
-- (fromList [],TInt)
--
-- Below is: (let [x (fn [y] y)] (x (x 3)))
-- >>> checkExp emptyTEnv (Let (Var "x") (Function (Var "y") (Var "y")) (UnaryCall (Var "x") (UnaryCall (Var "x") (Literal (IntV 3)))))
-- (fromList [],TInt)
--
-- Below is: (let [x (fn [y] y)] (x x))
-- >>> checkExp emptyTEnv (Let (Var "x") (Function (Var "y") (Var "y")) (UnaryCall (Var "x") (Var "x")))
-- (fromList [],TFun (TVar "y") (TVar "y"))
--
-- >>> checkExp emptyTEnv (UnaryCall (Var "x") (Literal (IntV 0)))
-- *** Exception: Unbounded variable
--
-- >>> checkExp (M.fromList [("x",TInt)]) (UnaryCall (Var "x") (Literal (IntV 0)))
-- *** Exception: calling a non-function
--
-- Below is:
--   Environment: x : (-> Bool Int)
--   (x 0)
--
-- >>> checkExp (M.fromList [("x",TFun TBool TInt)]) (UnaryCall (Var "x") (Literal (IntV 0)))
-- *** Exception: type mismatch: expecting TBool but got TInt
--
checkExp env (UnaryCall (Var fn) arg) =
  let (_, fnT) = checkExp env (Var fn)
  in case fnT of
       (TFun a b) ->
         let (_, argT) = checkExp env arg
         in if isBound a && a /= argT
            -- Function doesn't take the type given by the argument body:
            -- ((-> Bool Int) Int) => error
            then error $ "type mismatch: expecting " ++ show a ++ " but got " ++ show argT

            else if isFree a && a == b
            -- If a and b are the same free variable, return the argument type:
            -- ((-> a a) Int) => ((-> Int Int) Int) => Int
            then (env, argT)

            -- Else, return b whether free or bound:
            --
            -- ((-> a b) Int) => ((-> Int b) Int) => b
            -- ((-> a Bool) Int) => ((-> Int Bool) Int) => Bool
            --
            else (env, b)
       _ -> error "calling a non-function"

-- Below is: ((fn [x] x) 0)
-- >>> checkExp emptyTEnv (UnaryCall (Function (Var "x") (Var "x")) (Literal (IntV 0)))
-- (fromList [],TInt)
--
-- Below is: ((fn [x] (fn [y] x)) 0)
-- >>> checkExp emptyTEnv (UnaryCall (Function (Var "x") (Function (Var "y") (Var "x"))) (Literal (IntV 0)))
-- (fromList [],TFun (TVar "y") TInt)
--
-- Below is: ((fn [x] (fn [y] x)) 0 True)
-- >>> checkExp emptyTEnv (UnaryCall (UnaryCall (Function (Var "x") (Function (Var "y") (Var "x"))) (Literal (IntV 0))) (Literal (BoolV True)))
-- (fromList [],TFun (TVar "y") TInt)
--
checkExp env (UnaryCall (Function (Var v) body) arg) =
  let (env', argT) = checkExp env arg  -- Find argument body's type
      env'' = insertEnv env' v argT    -- Bind argument var's type to above
      (_, bodyT) = checkExp env'' body -- Check function body's type with new env
  in (env, bodyT)
-- checkExp env (UnaryCall expr arg) =
--   let (env', exprT) = checkExp env expr
--   in case exprT of
--        TFun a b -> _
--        _ -> error "calling a non-function"

-- TODO how to deal with a free var where there is no function argument name to
-- use as the variable name.
checkExp env (List []) = (env, TList TFree)
checkExp env (List [e]) =
  let (_, eT) = checkExp env e
  in (env, TList eT)
checkExp env (List (e1:e2:es)) =
  let (_, e1T) = checkExp env e1
      (_, e2T) = checkExp env e2
  in if e1T == e2T
       then checkExp env (List (e2:es))
       else error $ "list type mismatch error: expecting " ++ show e1T ++ " but got " ++ show e1T


