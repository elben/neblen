{-# LANGUAGE OverloadedStrings #-}

module Neblen.TypeChecker where

import Neblen.Data
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Except


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

-- | Get fresh variable.
--
-- >>> evalState getFresh initFreshCounter
-- "a"
--
-- >>> evalState getFresh (FreshCounter { getFreshCounter = 25 })
-- "z"
--
getFresh :: State FreshCounter TName
getFresh = do
  s <- get
  let c = getFreshCounter s
  return $ letters !! c

-- | Unify types.
--
-- >>> unify emptyUEnv TInt TInt
-- (fromList [],TInt)
--
-- >>> unify emptyUEnv TInt (TVar "a")
-- (fromList [("a",TInt)],TInt)
--
-- >>> unify (M.fromList [("a",TInt)]) (TVar "a") (TVar "b")
-- (fromList [("a",TInt),("b",TInt)],TInt)
--
-- >>> unify emptyUEnv (TFun TInt TInt) (TFun (TVar "a") (TVar "b"))
-- (fromList [("a",TInt),("b",TInt)],TFun TInt TInt)
--
-- >>> unify emptyUEnv (TFun TInt (TVar "a")) (TFun (TVar "a") (TVar "b"))
-- (fromList [("a",TInt),("b",TInt)],TFun TInt TInt)
--
-- >>> unify emptyUEnv (TFun TInt (TVar "a")) (TFun (TVar "b") (TVar "b"))
-- (fromList [("a",TInt),("b",TInt)],TFun TInt TInt)
--
-- >>> unify (M.fromList [("b",TBool)]) (TFun (TVar "a") (TVar "a")) (TFun (TVar "a") (TVar "b"))
-- (fromList [("a",TBool),("b",TBool)],TFun TBool TBool)
--
-- >>> unify (M.fromList [("a",TInt)]) (TFun (TVar "a") (TVar "b")) (TFun (TVar "a") (TVar "b"))
-- (fromList [("a",TInt)],TFun TInt (TVar "b"))
--
-- >>> unify (M.fromList [("b",TBool)]) (TFun (TVar "a") (TVar "b")) (TFun (TVar "c") (TVar "d"))
-- (fromList [("a",TVar "c"),("b",TBool),("c",TVar "a"),("d",TBool)],TFun (TVar "c") TBool)
--
-- >>> unify emptyUEnv (TFun (TVar "a") (TVar "a")) (TFun (TVar "a") (TVar "a"))
-- (fromList [],TFun (TVar "a") (TVar "a"))
--
-- Failure cases:
--
-- >>> unify (M.fromList [("a",TBool)]) (TVar "a") TInt
-- *** Exception: type mismatch: expecting TBool but got TInt
--
-- >>> unify emptyUEnv (TVar "a") (TFun (TVar "a") TInt)
-- *** Exception: infinite type: TVar "a" and TFun (TVar "a") TInt
--
-- >>> unify (M.fromList [("a",TBool)]) TInt (TVar "a")
-- *** Exception: type mismatch: expecting TBool but got TInt
--
-- >>> unify emptyUEnv (TVar "a") (TFun (TVar "a") TInt)
-- *** Exception: infinite type: TVar "a" and TFun (TVar "a") TInt
--
unify :: UEnv -> Type -> Type -> (UEnv, Type)
unify uenv TInt TInt = (uenv, TInt)
unify uenv TBool TBool = (uenv, TBool)
unify uenv TString TString = (uenv, TString)
unify uenv (TVar tv) t2 = unifyTVar uenv (TVar tv) t2
unify uenv t1 (TVar tv) = unifyTVar uenv (TVar tv) t1
unify uenv (TFun a1 r1) (TFun a2 r2) =
  let (uenv', ta) = unify uenv a1 a2
      (uenv'', tr) = unify uenv' r1 r2
  -- We now have a u-env built from both argument and return types. Substitue
  -- any remaining type variables.
  in (uenv'', TFun (replaceAllTVars uenv'' ta) (replaceAllTVars uenv'' tr))
unify _ t1 t2 = error $ "type mismatch: expecting " ++ show t1 ++ " but got " ++ show t2

unifyTVar :: UEnv -> Type -> Type -> (UEnv, Type)
unifyTVar uenv (TVar tv) t2 =
  case lookupEnv uenv tv of
    Nothing ->
      case t2 of
        -- If both sides are free, then see if we can resolve the RHS with the
        -- u-env. For example, in (-> a a) (-> a b) with uenv {b -> Int}, the
        -- LHS failed to resolve, so we try the RHS.
        TVar tv2 -> case lookupEnv uenv tv2 of
                      -- RHS is still free.
                      Nothing -> if tv == tv2
                                 -- Same free variable. Return this free
                                 -- variable.
                                 then (uenv, TVar tv2)

                                 else if occursCheck tv t2
                                 then error $ "infinite type: " ++ show (TVar tv) ++ " and " ++ show t2

                                 -- Unify free variables together.
                                 --
                                 -- Example: a <==> b.
                                 -- UEnv: {a -> b, b -> a}
                                 -- Returned: a
                                 else (insertEnv (insertEnv uenv tv (TVar tv2)) tv2 (TVar tv), TVar tv)

                      -- Resolved RHS. In the example above, b (RHS) is resolved to
                      -- Int. So we need to update uenv to {b -> Int, a -> Int}
                      Just t2' -> let uenv' = insertEnv uenv tv2 t2'
                                      uenv'' = insertEnv uenv' tv t2'
                                  in unify (insertEnv uenv'' tv2 t2') (TVar tv2) t2'
        _      -> if occursCheck tv t2
                  then error $ "infinite type: " ++ show (TVar tv) ++ " and " ++ show t2
                  else let uenv' = insertEnv uenv tv t2
                       in unify uenv' (TVar tv) t2
    Just t1 ->
      unify uenv t1 t2
unifyTVar _ _ _ = error "bad call to unifyTVar"

-- This occurs check asserts that if we are applying a substitution of variable
-- x to an expression e, the variable x cannot be free in e. Otherwise the
-- rewrite would diverge, constantly rewriting itself.
--
-- That is, an infinite type.
--
-- For example, this won't unify: a <==> (a -> b).
--
-- But note that this should not be used to check: a <==> a.  If the type
-- variables are the same, it is OK (it is not infinite, we can just replace 'a'
-- with any type once we resolve its type)
--
-- >>> occursCheck "a" (TFun TInt (TVar "a"))
-- True
--
-- >>> occursCheck "a" (TFun TInt (TVar "b"))
-- False
--
occursCheck :: TName -> Type -> Bool
occursCheck tv t = S.member tv (findAllTVars t)

  -- harvest all the free variables.
  -- permute through all possible configurations.
  --
--
-- TODO: This needs a unifier to realize that a = stillfree = TInt.
--
-- >>> checkExp (M.fromList [("x", TFun (TVar "a") (TVar "a"))]) emptyUEnv (UnaryCall (Function (Var "y") (UnaryCall (Var "y") (Literal (IntV 3)))) (Var "x"))
-- *** Exception: type mismatch: expecting TFun TInt (TVar "stillfree") but got TFun (TVar "a") (TVar "a")
-- ^ This should unify to: TFun TInt TInt

-- Below is: ((fn [x] x 3) (fn [x] x))
--                         (-> a a)
--            (fn [x : (-> a a)] x 3)
--
-- TODO: This needs a unifier:
--
-- >>> checkExp emptyTEnv (UnaryCall (Function (Var "x") emptyUEnv (UnaryCall (Var "x") (Literal (IntV 3)))) (Function (Var "x") (Var "x")))
-- *** Exception: type mismatch: expecting TFun TInt (TVar "stillfree") but got TFun (TVar "x") (TVar "x")
--

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

findAllTVars :: Type -> S.Set TName
findAllTVars (TVar t) = S.singleton t
findAllTVars (TFun a r) = S.union (findAllTVars a) (findAllTVars r)
findAllTVars (TList t) = findAllTVars t
findAllTVars (TVec t) = findAllTVars t
findAllTVars _ = S.empty

replaceAllTVars :: UEnv -> Type -> Type
replaceAllTVars uenv (TVar tv) = fromMaybe (TVar tv) (lookupEnv uenv tv)
replaceAllTVars uenv (TFun a r) = TFun (replaceAllTVars uenv a) (replaceAllTVars uenv r)
replaceAllTVars _ t = t

typeVarName :: Type -> Name
typeVarName (TVar n) = n
typeVarName _ = error "Invalid call."

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Map.Strict as M

emptyTEnv :: TEnv
emptyTEnv = M.empty

emptyUEnv :: UEnv
emptyUEnv = M.empty

lookupEnv :: TEnv -> Name -> Maybe Type
lookupEnv tenv name = M.lookup name tenv

insertEnv :: TEnv -> Name -> Type -> TEnv
insertEnv tenv name t = M.insert name t tenv

-- >>> check emptyTEnv emptyUEnv (Literal (IntV 0))
-- (fromList [],fromList [],TInt)
--
-- >>> check emptyTEnv emptyUEnv (Literal (BoolV True))
-- (fromList [],fromList [],TBool)
--
-- >>> check emptyTEnv emptyUEnv (Literal (StringV "Hello"))
-- (fromList [],fromList [],TString)
--
checkLiteral :: TEnv -> UEnv -> Exp -> TypeCheck
checkLiteral tenv uenv (Literal (IntV _)) = return (tenv, uenv, TInt)
checkLiteral tenv uenv (Literal (BoolV _)) = return (tenv, uenv, TBool)
checkLiteral tenv uenv (Literal (StringV _)) = return (tenv, uenv, TString)
checkLiteral _ _ _ = throwError emptyGenericTypeError

-- | Check variable.
--
-- >>> check (M.fromList [("x",TInt)]) emptyUEnv (Var "x")
-- (fromList [("x",TInt)],fromList [],TInt)
--
-- >>> check emptyTEnv emptyUEnv (Var "x")
-- (fromList *** Exception: unbounded variable
--
checkVar :: TEnv -> UEnv -> Exp -> TypeCheck
checkVar tenv uenv (Var v) =
  case lookupEnv tenv v of
    Just t  -> return (tenv, uenv, t)
    Nothing -> throwError (UnboundVariable v)
checkVar _ _ _ = error "wrong type"

-- | Check let.
--
-- >>> check emptyTEnv emptyUEnv (Let (Var "x") (Literal (IntV 0)) (Var "x"))
-- (fromList [],fromList [],TInt)
--
-- >>> check emptyTEnv emptyUEnv (Let (Var "x") (Literal (IntV 0)) (Literal (BoolV True)))
-- (fromList [],fromList [],TBool)
--
-- >>> check (M.fromList [("b",TBool)]) emptyUEnv (Let (Var "x") (Literal (IntV 0)) (Var "b"))
-- (fromList [("b",TBool)],fromList [],TBool)
--
checkLet :: TEnv -> UEnv -> Exp -> TypeCheck
checkLet tenv uenv (Let (Var v) val body) = do
  (tenv', uenv', valT) <- checkExp tenv uenv val
  let tenv'' = insertEnv tenv' v valT
  (_, uenv'', valB) <- checkExp tenv'' uenv' body
  -- Return original tenv because 'v' is no longer in scope.
  return (tenv, uenv'', valB)
checkLet _ _ _ = throwError emptyGenericTypeError

-- | Check nullary function.
--
-- >>> check emptyTEnv emptyUEnv (NullaryFun (Literal (IntV 0)))
-- (fromList [],fromList [],TInt)
--
-- >>> check emptyTEnv emptyUEnv (NullaryFun (Var "x"))
-- (fromList *** Exception: unbounded variable
--
checkNullFun :: TEnv -> UEnv -> Exp -> TypeCheck
checkNullFun tenv uenv (NullaryFun body) = checkExp tenv uenv body
checkNullFun _ _ _ = throwError emptyGenericTypeError

-- | Check function.
--
-- >>> check emptyTEnv emptyUEnv (Function (Var "x") (Var "x"))
-- (fromList [],fromList [],TFun (TVar "x") (TVar "x"))
--
-- >>> check emptyTEnv emptyUEnv (Function (Var "x") (Literal (IntV 0)))
-- (fromList [],fromList [],TFun (TVar "x") TInt)
--
-- >>> check emptyTEnv emptyUEnv (Function (Var "x") (Function (Var "y") (Literal (IntV 0))))
-- (fromList [],fromList [],TFun (TVar "x") (TFun (TVar "y") TInt))
--
-- Verify that argument variable doesn't override outside scope:
--   Environment: x : Bool
--   (fn [x] (fn [y] 0))
--
-- >>> check (M.fromList [("x",TBool)]) emptyUEnv (Function (Var "x") (Function (Var "y") (Literal (IntV 0))))
-- (fromList [("x",TBool)],fromList [],TFun (TVar "x") (TFun (TVar "y") TInt))
--
-- Below is: (fn [x] (fn [y] x))
-- >>> check emptyTEnv emptyUEnv (Function (Var "x") (Function (Var "y") (Var "x")))
-- (fromList [],fromList [],TFun (TVar "x") (TFun (TVar "y") (TVar "x")))
--
-- Below is: (fn [x] x x)
-- >>> check emptyTEnv emptyUEnv (Function (Var "x") (UnaryCall (Var "x") (Var "x")))
-- (fromList [],fromList [],TFun (TVar "x") (TFun (TVar "x") (TVar "x")))
--
-- Below is: (fn [x] x 3)
-- >>> check emptyTEnv emptyUEnv (Function (Var "x") (UnaryCall (Var "x") (Literal (IntV 3))))
-- (fromList [],fromList [],TFun (TFun TInt (TVar "stillfree")) (TVar "stillfree"))
--
checkFun :: TEnv -> UEnv -> Exp -> TypeCheck
checkFun tenv uenv (Function (Var v) body) = do
  let tenv' = insertEnv tenv v (TVar v) -- Set argument as free (TODO use getFresh, and insert mapping fresh -> (TVar v))
  (tenv'', uenv', bodyT) <- checkExp tenv' uenv body -- Check body
  -- May have figured out argument's type when body was checked.
  let argT = fromMaybe (TVar v) (lookupEnv tenv'' v)
  return (tenv, uenv', TFun (replaceAllTVars uenv' argT) bodyT)
checkFun _ _ _ = throwError emptyGenericTypeError

-- | Check nullary function call.
--
-- >>> check (M.fromList [("x",TBool)]) emptyUEnv (NullaryCall (Var "x"))
-- (fromList [("x",TBool)],fromList [],TBool)
--
-- >>> check emptyTEnv emptyUEnv (NullaryCall (NullaryFun (Literal (BoolV True))))
-- (fromList [],fromList [],TBool)
--
-- >>> check emptyTEnv emptyUEnv (NullaryCall (Var "x"))
-- (fromList *** Exception: unbounded variable
--
checkNullCall :: TEnv -> UEnv -> Exp -> TypeCheck
checkNullCall tenv uenv (NullaryCall (Var v)) = checkExp tenv uenv (Var v)
checkNullCall tenv uenv (NullaryCall (NullaryFun body)) = checkExp tenv uenv (NullaryFun body)
checkNullCall _ _ _ = throwError emptyGenericTypeError

-- | Check unary function call.
--
-- >>> check (M.fromList [("x",TFun TInt TBool)]) emptyUEnv (UnaryCall (Var "x") (Literal (IntV 0)))
-- (fromList [("x",TFun TInt TBool)],fromList [],TBool)
--
-- Below is the case:
--   Environment:
--     f : (-> Bool Bool)
--   (let [x true] (f x))
--
-- >>> check (M.fromList [("f",TFun TBool TBool)]) emptyUEnv (Let (Var "x") (Literal (BoolV True)) (UnaryCall (Var "f") (Var "x")))
-- (fromList [("f",TFun TBool TBool)],fromList [],TBool)
--
-- Below is the case:
--   Environment:
--     f : (-> Int Bool)
--   ((fn [x] x) f)
--
-- >>> check (M.fromList [("f",TFun TInt TBool)]) emptyUEnv (UnaryCall (Function (Var "x") (Var "x")) (Var "f"))
-- (fromList [("f",TFun TInt TBool)],fromList [("x",TFun TInt TBool)],TFun TInt TBool)
--
-- Below is: (let [x (fn [y] y)] (x 3))
-- >>> check emptyTEnv emptyUEnv (Let (Var "x") (Function (Var "y") (Var "y")) (UnaryCall (Var "x") (Literal (IntV 3))))
-- (fromList [],fromList [("y",TInt)],TInt)
--
-- Below is: (let [x (fn [y] y)] (x (x 3)))
-- >>> check emptyTEnv emptyUEnv (Let (Var "x") (Function (Var "y") (Var "y")) (UnaryCall (Var "x") (UnaryCall (Var "x") (Literal (IntV 3)))))
-- (fromList [],fromList [("y",TInt)],TInt)
--
-- Below is: ((fn [x] x) 0)
-- >>> check emptyTEnv emptyUEnv (UnaryCall (Function (Var "x") (Var "x")) (Literal (IntV 0)))
-- (fromList [],fromList [("x",TInt)],TInt)
--
-- Below is: ((fn [x] (fn [y] x)) 0)
-- >>> check emptyTEnv emptyUEnv (UnaryCall (Function (Var "x") (Function (Var "y") (Var "x"))) (Literal (IntV 0)))
-- (fromList [],fromList [("x",TInt)],TFun (TVar "y") TInt)
--
-- Below is: ((fn [x] (fn [y] x)) 0 True)
-- >>> check emptyTEnv emptyUEnv (UnaryCall (UnaryCall (Function (Var "x") (Function (Var "y") (Var "x"))) (Literal (IntV 0))) (Literal (BoolV True)))
-- (fromList [],fromList [("x",TInt),("y",TBool)],TInt)
--
-- Below is: ((fn [x] x 3) (fn [x] x))
-- >>> check emptyTEnv emptyUEnv (UnaryCall (Function (Var "x") (UnaryCall (Var "x") (Literal (IntV 3)))) (Function (Var "x") (Var "x")))
-- (fromList [],fromList [("stillfree",TInt),("x",TInt)],TInt)
--
-- >>> check emptyTEnv emptyUEnv (UnaryCall (Var "x") (Literal (IntV 0)))
-- (fromList *** Exception: unbounded variable
--
-- >>> check (M.fromList [("x",TInt)]) emptyUEnv (UnaryCall (Var "x") (Literal (IntV 0)))
-- (fromList *** Exception: calling a non-function
--
-- Below is: (let [x (fn [y] y)] (x x))
-- >>> check emptyTEnv emptyUEnv (Let (Var "x") (Function (Var "y") (Var "y")) (UnaryCall (Var "x") (Var "x")))
-- (fromList [],fromList *** Exception: infinite type: TVar "y" and TFun (TVar "y") (TVar "y")
--
-- Below is:
--   Environment: x : (-> Bool Int)
--   (x 0)
--
-- >>> check (M.fromList [("x",TFun TBool TInt)]) emptyUEnv (UnaryCall (Var "x") (Literal (IntV 0)))
-- (fromList [("x",TFun TBool TInt)],fromList *** Exception: type mismatch: expecting TBool but got TInt
--
-- Below is:
--   Environment:
--     x : (-> a a)
--  ((fn [y] y 3) x) : Int
--
-- >>> check (M.fromList [("x", TFun (TVar "a") (TVar "a"))]) emptyUEnv (UnaryCall (Function (Var "y") (UnaryCall (Var "y") (Literal (IntV 3)))) (Var "x"))
-- (fromList [("x",TFun (TVar "a") (TVar "a"))],fromList [("a",TInt),("stillfree",TInt)],TInt)
--
-- Below is: ((fn [x] x 3) (fn [x] x))
--                         (-> a a)
--            (fn [x : (-> a a)] x 3) : Int
--
-- TODO: This needs a unifier:
--
-- >>> check emptyTEnv emptyUEnv (UnaryCall (Function (Var "x") (UnaryCall (Var "x") (Literal (IntV 3)))) (Function (Var "x") (Var "x")))
-- (fromList [],fromList [("stillfree",TInt),("x",TInt)],TInt)
--
-- Below is self application (see Pierce pg 345):
--   Environment: f : (-> a a)
--   ((fn [x] x x) f) : (-> (-> a a) (-> a a))
--
--   ((fn [x] x x) f)
--     : (-> c c) f          uenv: {c = x}         env: {f = (-> a a)}
--       Look up 'f' in env
--     = (-> c c) (-> a a)   uenv: {c = x}         env: {f = (-> a a)}
--       Apply function
--     = (-> a a) c          uenv: {c = (-> a a)}  env: {f = (-> a a), x = (-> a a)}
--       Lookup 'c'
--     = (-> a a) (-> a a)   uenv: {c = (-> a a)}  env: {f = (-> a a), x = (-> a a)}
--         ^ This is a problem! This is an infinite type.
--
--    What we want is this:
--
--       Give different fresh variables to argument and return types.
--     : (-> c d) f          uenv: {c = x, d = x}         env: {f = (-> a a)}
--       Look up 'f' in env, replace 'x'
--     = (-> c d) (-> a a)   uenv: {c = x, d = x}         env: {x = (-> a a), f = (-> a a)}
--       Apply function
--     = (-> a a) d          uenv: {c = (-> a a), d = x}  env: {f = (-> a a), x = (-> a a)}
--       Lookup 'd'
--     = (-> a a) (-> b b)   uenv: {c = (-> a a), d = (-> b b)}  env: {f = (-> a a), x = (-> a a)}
--
--         ^ The key is that 'f' is polymorphic, and we should get fresh variables
--         for each use of f. That is, we should have (-> a a) and (-> b b).
--
--     = (-> (-> b b) (-> b b))
--
--   It USED to work. But now it's an infinite type error. Is this correct?
--
--   In Haskell:
--
--     Allowed: id id
--
--     Not allowed: ((\x -> x x) id)
--
-- >>> check (M.fromList [("f",TFun (TVar "a") (TVar "a"))]) emptyUEnv (UnaryCall (Function (Var "x") (UnaryCall (Var "x") (Var "x"))) (Var "f"))
-- (fromList [("f",TFun (TVar "a") (TVar "a"))],fromList [],TFun (TFun (TVar "a") (TVar "a")) (TFun (TVar "a") (TVar "a")))
--
checkUnaryCall :: TEnv -> UEnv -> Exp -> TypeCheck
checkUnaryCall tenv uenv (UnaryCall fn arg) = do
  (_, uenv', fnT) <- checkExp tenv uenv fn
  case fnT of
    (TFun fna fnb) -> do
      (_, uenv'', argT) <- checkExp tenv uenv' arg
      let (uenv''', _) = unify uenv'' fna argT
      return (tenv, uenv''', replaceAllTVars uenv''' fnb)
      -- in if isBound fna && fna /= argT
      --    -- Function doesn't take the type given by the argument body:
      --    -- ((-> Bool Int) Int) => error
      --    then error $ "type mismatch: expecting " ++ show fna ++ " but got " ++ show argT

      --    else if isFree fna && fna == fnb
      --    -- If fna and fnb are the same free variable, return the argument type:
      --    -- ((-> fna fna) Int) => ((-> Int Int) Int) => Int
      --    then (tenv, argT)

      --    else if isFree fna && isBound argT
      --    -- If fna is free and arg is bound, bound fna to that type:
      --    -- ((-> fna (-> fnb fna)) Int) => ((-> Int (-> fnb Int)) Int) => (-> fnb Int)
      --    then let fnb' = bindFree (typeVarName fna) argT fnb in (tenv, fnb')

      --    -- Else, return fnb whether free or bound:
      --    --
      --    -- ((-> fna fnb) Int) => ((-> Int fnb) Int) => fnb
      --    -- ((-> fna Bool) Int) => ((-> Int Bool) Int) => Bool
      --    --
      --    else (tenv, fnb)

    -- The function type is still free. We can bound the function argument to
    -- the argument type, but we don't know what the resulting type is. What
    -- we need is a type hint from the user.
    --
    -- Possibilities (given x and y are free):
    --
    --   (x 0) - x : (-> Int ?)
    --   (x e) - x : (-> (typeOf e) ?)
    --
    --   (x x) - x : (-> x ?)
    --   (x y) - x : (-> y ?)
    --
    -- We can't do this in Haskell either (occur typing):
    --
    --   foo bar = bar bar
    --
    --     bar : a -> b
    --     foo: (a -> b) -> ???
    --
    (TVar vT) -> do
      (_, uenv'', argT) <- checkExp tenv uenv' arg
      if isBound argT
      then let retT = TVar "stillfree"
               -- We partially know the type of the function. Add that to
               -- the tenv so that parent can use it in their type check.
               tenv' = insertEnv tenv vT (TFun argT retT)
           in return (tenv', uenv'', retT)
      else return (tenv, uenv'', TFun (TVar "free") (TVar "free"))
    t -> (do
      -- Find argument type for better error message.
      -- TODO: getFresh
      (_, _, argT) <- checkExp tenv uenv' arg
      throwError (Mismatch (TFun argT (TVar "free")) t))
checkUnaryCall _ _ _ = throwError emptyGenericTypeError

-- | Check list.
--
-- >>> check emptyTEnv emptyUEnv (List [Literal (IntV 0)])
-- (fromList [],fromList [],TList TInt)
--
-- Below is: (list 0 ((fn [x] x) 0))
-- >>> check emptyTEnv emptyUEnv (List [Literal (IntV 0),UnaryCall (Function (Var "x") (Var "x")) (Literal (IntV 0))])
-- (fromList [],fromList [("x",TInt)],TList TInt)
--
-- >>> check emptyTEnv emptyUEnv (List [Literal (IntV 0),Literal (BoolV True)])
-- (fromList *** Exception: list type mismatch: TInt and TBool found
--
-- >>> check emptyTEnv emptyUEnv (List [])
-- TODO: need to use a TVar with non-clashing variable name.
--
checkList :: TEnv -> UEnv -> Exp -> TypeCheck
checkList tenv uenv (List []) = return (tenv, uenv, TList (TVar "free"))
checkList tenv uenv (List [e]) = do
  (_, uenv', eT) <- checkExp tenv uenv e
  return (tenv, uenv', TList eT)
checkList tenv uenv (List (e1:e2:es)) = do
  (_, uenv', e1T) <- checkExp tenv uenv e1
  (_, uenv'', e2T) <- checkExp tenv uenv' e2
  if e1T == e2T
     then checkList tenv uenv'' (List (e2:es))
     else error $ "list type mismatch: " ++ show e1T ++ " and " ++ show e2T ++ " found"
checkList _ _ _ = error "wrong type"

-- | Check vector.
--
-- TODO use vectors but still share code with checkList.
checkVector :: TEnv -> UEnv -> Exp -> TypeCheck
checkVector = checkList

-- | Check if.
--
-- Below is: (if ((fn [x] true) 0) "truth" "false") : String
-- >>> check emptyTEnv emptyUEnv (If (UnaryCall (Function (Var "x") (Literal (BoolV True))) (Literal (IntV 0))) (Literal (StringV "then clause")) (Literal (StringV "else clause")))
-- (fromList [],fromList [("x",TInt)],TString)
--
-- Below is: (if false 0 false)
-- >>> check emptyTEnv emptyUEnv (If (Literal (BoolV False)) (Literal (IntV 0)) (Literal (BoolV False)))
-- (fromList *** Exception: then and else clause type mismatch: TInt and TBool found
--
checkIf :: TEnv -> UEnv -> Exp -> TypeCheck
checkIf tenv uenv (If p t e) = do
  (_, uenv', pT) <- checkExp tenv uenv p
  case pT of
    TBool -> do
      (_, uenv'', tT) <- checkExp tenv uenv' t
      (_, uenv''', eT) <- checkExp tenv uenv'' e
      if tT == eT
      then return (tenv, uenv''', eT)
      else error $ "then and else clause type mismatch: " ++ show tT ++ " and " ++ show eT ++ " found"
    _     ->
      error "predicate must be a boolean"
checkIf _ _ _ = error "wrong type"

-- | Type check expression.
--
-- -- TODO: Convert to monad transformer so that TEnv is passed along
-- TODO: Convert to monad so we can pass a State along that contains the free
-- variable generator counter.
--
-- >>> check emptyTEnv emptyUEnv (Literal (IntV 0))
-- (fromList [],fromList [],TInt)
--
-- >>> check emptyTEnv emptyUEnv (Let (Var "x") (Literal (IntV 0)) (Var "x"))
-- (fromList [],fromList [],TInt)
--
-- >>> check emptyTEnv emptyUEnv (NullaryFun (Literal (IntV 0)))
-- (fromList [],fromList [],TInt)
--
-- >>> check emptyTEnv emptyUEnv (Function (Var "x") (Var "x"))
-- (fromList [],fromList [],TFun (TVar "x") (TVar "x"))
--
-- >>> check emptyTEnv emptyUEnv (If (Literal (BoolV False)) (Literal (IntV 0)) (Literal (IntV 0)))
-- (fromList [],fromList [],TInt)
--
checkExp :: TEnv -> UEnv -> Exp -> TypeCheck
checkExp tenv uenv (Literal lit) = checkLiteral tenv uenv (Literal lit)
checkExp tenv uenv (Var v) = checkVar tenv uenv (Var v)
checkExp tenv uenv e@(Let {}) = checkLet tenv uenv e
checkExp tenv uenv e@(NullaryFun {}) = checkNullFun tenv uenv e
checkExp tenv uenv e@(Function {}) = checkFun tenv uenv e
checkExp tenv uenv e@(NullaryCall {}) = checkNullCall tenv uenv e
checkExp tenv uenv e@(UnaryCall {}) = checkUnaryCall tenv uenv e
checkExp tenv uenv e@(List {}) = checkList tenv uenv e
checkExp tenv uenv e@(Vector {}) = checkVector tenv uenv e
checkExp tenv uenv e@(If {}) = checkIf tenv uenv e
checkExp _ _ (Def {}) = error "TODO"
checkExp _ _ (Add {}) = error "TODO"

check :: TEnv -> UEnv -> Exp -> (TEnv, UEnv, Type)
check tenv uenv expr =
  case evalState (runExceptT (checkExp tenv uenv expr)) initFreshCounter of
    Left e -> error (show e)
    Right r -> r
