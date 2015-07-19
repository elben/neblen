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

-- | Bind free type variables to the given type.
--
-- >>> bindFree "x" TInt (TVar "x")
-- TInt
--
-- >>> bindFree "x" TInt (TVar "y")
-- TVar "y"
--
-- >>> bindFree "x" TInt (TFun (TVar "x") (TVar "y"))
-- TFun TInt (TVar "y")
--
-- >>> bindFree "x" TInt (TList (TVar "x"))
-- TList TInt
--
-- >>> bindFree "x" TInt (TList (TVar "y"))
-- TList (TVar "y")
--
-- >>> bindFree "x" TInt (TVec (TVar "x"))
-- TVec TInt
--
bindFree :: Name -> Type -> Type -> Type
bindFree name t (TVar n) = if name == n then t else TVar n
bindFree name t (TFun a b) = TFun (bindFree name t a) (bindFree name t b)
bindFree name t (TList t') = TList (bindFree name t t')
bindFree name t (TVec t') = TVec (bindFree name t t')
bindFree _ _ t = t

typeVarName :: Type -> Name
typeVarName (TVar n) = n
typeVarName _ = error "Invalid call."

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Map.Strict as M

emptyTEnv :: TEnv
emptyTEnv = M.empty

lookupEnv :: TEnv -> Name -> Maybe Type
lookupEnv env name = M.lookup name env

insertEnv :: TEnv -> Name -> Type -> TEnv
insertEnv env name t = M.insert name t env

-- >>> checkExp emptyTEnv (Literal (IntV 0))
-- (fromList [],TInt)
--
-- >>> checkExp emptyTEnv (Literal (BoolV 0))
-- (fromList [],TBool)
--
-- >>> checkExp emptyTEnv (Literal (StringV 0))
-- (fromList [],TString)
--
checkLiteral :: TEnv -> Exp -> (TEnv, Type)
checkLiteral env (Literal (IntV _)) = (env, TInt)
checkLiteral env (Literal (BoolV _)) = (env, TBool)
checkLiteral env (Literal (StringV _)) = (env, TString)
checkLiteral _ _ = error "Invalid"

-- | Check variable.
--
-- >>> checkExp (M.fromList [("x",TInt)]) (Var "x")
-- (fromList [("x",TInt)],TInt)
--
-- >>> checkExp emptyTEnv (Var "x")
-- *** Exception: unbounded variable
--
checkVar :: TEnv -> Exp -> (TEnv, Type)
checkVar env (Var v) =
  case lookupEnv env v of
    Just t  -> (env, t)
    Nothing -> error "unbounded variable"
checkVar _ _ = error "wrong type"

-- | Check let.
--
-- >>> checkLet emptyTEnv (Let (Var "x") (Literal (IntV 0)) (Var "x"))
-- (fromList [],TInt)
--
-- >>> checkLet emptyTEnv (Let (Var "x") (Literal (IntV 0)) (Literal (BoolV True)))
-- (fromList [],TBool)
--
-- >>> checkLet (M.fromList [("b",TBool)]) (Let (Var "x") (Literal (IntV 0)) (Var "b"))
-- (fromList [("b",TBool)],TBool)
--
checkLet :: TEnv -> Exp -> (TEnv, Type)
checkLet env (Let (Var v) val body) =
  let (env', valT) = checkExp env val
      env'' = insertEnv env' v valT
      (_, valB) = checkExp env'' body
  -- Return original env because 'v' is no longer in scope.
  in (env, valB)
checkLet _ _ = error "wrong type"

-- | Check nullary function.
--
-- >>> checkNullFun emptyTEnv (NullaryFun (Literal (IntV 0)))
-- (fromList [],TInt)
--
-- >>> checkNullFun emptyTEnv (NullaryFun (Var "x"))
-- *** Exception: unbounded variable
--
checkNullFun :: TEnv -> Exp -> (TEnv, Type)
checkNullFun env (NullaryFun body) = checkExp env body
checkNullFun _ _ = error "wrong type"

-- | Check function.
--
-- >>> checkFun emptyTEnv (Function (Var "x") (Var "x"))
-- (fromList [],TFun (TVar "x") (TVar "x"))
--
-- >>> checkFun emptyTEnv (Function (Var "x") (Literal (IntV 0)))
-- (fromList [],TFun (TVar "x") TInt)
--
-- >>> checkFun emptyTEnv (Function (Var "x") (Function (Var "y") (Literal (IntV 0))))
-- (fromList [],TFun (TVar "x") (TFun (TVar "y") TInt))
--
-- Verify that argument variable doesn't override outside scope:
--   Environment: x : Bool
--   (fn [x] (fn [y] 0))
--
-- >>> checkFun (M.fromList [("x",TBool)]) (Function (Var "x") (Function (Var "y") (Literal (IntV 0))))
-- (fromList [("x",TBool)],TFun (TVar "x") (TFun (TVar "y") TInt))
--
-- Below is: (fn [x] (fn [y] x))
-- >>> checkFun emptyTEnv (Function (Var "x") (Function (Var "y") (Var "x")))
-- (fromList [],TFun (TVar "x") (TFun (TVar "y") (TVar "x")))
--
checkFun :: TEnv -> Exp -> (TEnv, Type)
checkFun env (Function (Var v) body) =
  let env' = insertEnv env v (TVar v) -- Set argument as free
      (_, bodyT) = checkExp env' body -- Check body
  in (env, TFun (TVar v) bodyT)
checkFun _ _ = error "wrong type"

-- | Check nullary function call.
--
-- >>> checkNullCall (M.fromList [("x",TBool)]) (NullaryCall (Var "x"))
-- (fromList [("x",TBool)],TBool)
--
-- >>> checkNullCall emptyTEnv (NullaryCall (NullaryFun (Literal (BoolV True))))
-- (fromList [],TBool)
--
-- >>> checkNullCall emptyTEnv (NullaryCall (Var "x"))
-- *** Exception: unbounded variable
--
checkNullCall :: TEnv -> Exp -> (TEnv, Type)
checkNullCall env (NullaryCall (Var v)) = checkExp env (Var v)
checkNullCall env (NullaryCall (NullaryFun body)) = checkExp env (NullaryFun body)
checkNullCall _ _ = error "wrong type"

-- | Check unary function call.
--
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
-- (fromList [],TInt)
--
-- >>> checkExp emptyTEnv (UnaryCall (Var "x") (Literal (IntV 0)))
-- *** Exception: unbounded variable
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
checkUnaryCall :: TEnv -> Exp -> (TEnv, Type)
checkUnaryCall env (UnaryCall fn arg) =
  let (_, fnT) = checkExp env fn
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

            else if isFree a && isBound argT
            -- If a is free and arg is bound, bound a to that type:
            -- ((-> a (-> b a)) Int) => ((-> Int (-> b Int)) Int) => (-> b Int)
            then let b' = bindFree (typeVarName a) argT b in (env, b')

            -- Else, return b whether free or bound:
            --
            -- ((-> a b) Int) => ((-> Int b) Int) => b
            -- ((-> a Bool) Int) => ((-> Int Bool) Int) => Bool
            --
            else (env, b)
       _ -> error "calling a non-function"
checkUnaryCall _ _ = error "wrong type"

-- | Check list.
--
checkList :: TEnv -> Exp -> (TEnv, Type)
checkList env (List []) = (env, TList TFree)
checkList env (List [e]) =
  let (_, eT) = checkList env e
  in (env, TList eT)
checkList env (List (e1:e2:es)) =
  let (_, e1T) = checkList env e1
      (_, e2T) = checkList env e2
  in if e1T == e2T
       then checkList env (List (e2:es))
       else error $ "list type mismatch error: expecting " ++ show e1T ++ " but got " ++ show e1T
checkList _ _ = error "wrong type"

-- | Check vector.
--
-- TODO use vectors but still share code with checkList.
checkVector :: TEnv -> Exp -> (TEnv, Type)
checkVector = checkList

-- | Type check expression.
--
-- -- TODO: Convert to monad transformer so that TEnv is passed along
--
-- >>> checkExp emptyTEnv (Literal (IntV 0))
-- (fromList [],TInt)
--
-- >>> checkExp emptyTEnv (Let (Var "x") (Literal (IntV 0)) (Var "x"))
-- (fromList [],TInt)
--
-- >>> checkExp emptyTEnv (NullaryFun (Literal (IntV 0)))
-- (fromList [],TInt)
--
-- >>> checkExp emptyTEnv (Function (Var "x") (Var "x"))
-- (fromList [],TFun (TVar "x") (TVar "x"))
--
checkExp :: TEnv -> Exp -> (TEnv, Type)
checkExp env (Literal lit) = checkLiteral env (Literal lit)
checkExp env (Var v) = checkVar env (Var v)
checkExp env e@(Let {}) = checkLet env e
checkExp env e@(NullaryFun {}) = checkNullFun env e
checkExp env e@(Function {}) = checkFun env e
checkExp env e@(NullaryCall {}) = checkNullCall env e
checkExp env e@(UnaryCall {}) = checkUnaryCall env e
checkExp env e@(List {}) = checkList env e
checkExp env e@(Vector {}) = checkVector env e
checkExp _ (Def {}) = error "TODO"
checkExp _ (Add {}) = error "TODO"



