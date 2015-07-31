{-# LANGUAGE OverloadedStrings #-}

module Neblen.TypeChecker where

import Neblen.Data
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Map.Strict as M

-- | Get fresh variable.
--
-- >>> evalState (runExceptT getFresh) initFreshCounter
-- Right a
--
-- >>> evalState (runExceptT getFresh) (FreshCounter { getFreshCounter = 1 })
-- Right b
--
getFresh :: ExceptT TypeError (State FreshCounter) Type
getFresh = do
  s <- lift get -- Same as: ExceptT (liftM Right get)
  lift $ put s{getFreshCounter = getFreshCounter s + 1}
  return $ TVar (letters !! getFreshCounter s)

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
--       a
--
type TypeCheck a = ExceptT TypeError (State FreshCounter) a

data Type = TInt
          | TBool
          | TString
          | TFun Type Type
          | TList Type
          | TVar TName
  deriving (Eq)

instance Show Type where
  show TInt = "Int"
  show TBool = "Bool"
  show TString = "String"
  show (TFun a r) = "(-> " ++ show a ++ " " ++ show r ++ ")"
  show (TList a) = "List " ++ show a
  show (TVar n) = n

-- Type schemes is a way to allow let-polymorphism (ML-style polymorphism):
-- functions can be instantiated with different types in the same body. See
-- Pierce 22.7 Let-Polymorphism (pg 331).
data TypeScheme = Forall [TName] Type

data TypeError = Mismatch Type Type
               | UnboundVariable Name
               | InfiniteType Type Type -- InfiniteType TVar Type
               | GenericTypeError (Maybe String)
  deriving (Eq)


emptyGenericTypeError :: TypeError
emptyGenericTypeError = GenericTypeError Nothing

genericTypeError :: String -> TypeError
genericTypeError msg = GenericTypeError (Just msg)

instance Show TypeError where
  show (Mismatch t1 t2) = "type mismatch: expecting " ++ show t1 ++ " but got " ++ show t2
  show (UnboundVariable n) = "unbound variable " ++ n
  show (InfiniteType tvar t) = "cannot resolve infinite type " ++ show tvar ++ " in " ++ show t
  show (GenericTypeError (Just msg)) = "type error: " ++ msg
  show (GenericTypeError Nothing) = "type error"

newtype FreshCounter = FreshCounter { getFreshCounter :: Int }

initFreshCounter :: FreshCounter
initFreshCounter = FreshCounter { getFreshCounter = 0 }

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

-- | Unify types.
--
unify :: UEnv -> Type -> Type -> TypeCheck (UEnv, Type)
unify uenv TInt TInt = return (uenv, TInt)
unify uenv TBool TBool = return (uenv, TBool)
unify uenv TString TString = return (uenv, TString)
unify uenv (TVar tv) t2 = unifyTVar uenv (TVar tv) t2
unify uenv t1 (TVar tv) = unifyTVar uenv (TVar tv) t1
unify uenv (TFun a1 r1) (TFun a2 r2) = do
  (uenv', ta) <- unify uenv a1 a2
  (uenv'', tr) <- unify uenv' (replaceAllTVars uenv' r1) (replaceAllTVars uenv' r2)
  -- We now have a u-env built from both argument and return types. Substitue
  -- any remaining type variables.
  return (uenv'', TFun (replaceAllTVars uenv'' ta) (replaceAllTVars uenv'' tr))
unify _ t1 t2 = throwE $ Mismatch t1 t2

unifyTVar :: UEnv -> Type -> Type -> TypeCheck (UEnv, Type)
unifyTVar uenv (TVar tv) t2 =
  case lookupEnv uenv tv of
    Nothing ->
      case t2 of
        -- If both sides are free, then see if we can resolve the RHS with the
        -- u-env. For example, in (-> a a) (-> a b) with uenv {b -> Int}, the
        -- LHS failed to resolve, so we try the RHS.
        TVar tv2 -> case lookupEnv uenv tv2 of
                      -- RHS is still free.
                      Nothing | tv == tv2  ->
                                  -- Same free variable. Return this free
                                  -- variable.
                                  return (uenv, TVar tv)

                              | occursCheck tv t2 -> throwE $ InfiniteType (TVar tv) t2
                              | otherwise ->
                                  -- Unify free variables together.
                                  --
                                  -- Example: a <==> b
                                  -- UEnv: {b -> a}
                                  -- Returned: a
                                  return (insertEnv uenv tv2 (TVar tv), TVar tv)
                                  -- return (insertEnv (insertEnv uenv tv (TVar tv2)) tv2 (TVar tv), TVar tv)

                      -- Resolved RHS. In the example above, b (RHS) is resolved to
                      -- Int. So we need to update uenv to {b -> Int, a -> Int}
                      Just t2' -> let uenv' = insertEnv uenv tv2 t2'
                                      uenv'' = insertEnv uenv' tv t2'
                                  in unify (insertEnv uenv'' tv2 t2') (TVar tv2) t2'
        _      -> if occursCheck tv t2
                  then throwE $ InfiniteType (TVar tv) t2
                  else let uenv' = insertEnv uenv tv t2
                       in unify uenv' (TVar tv) t2
    Just t1 ->
      unify uenv t1 t2
unifyTVar _ _ _ = error "bad call to unifyTVar"

runUnify :: TypeCheck (UEnv, Type) -> Either TypeError (UEnv, Type)
runUnify uc = evalState (runExceptT uc) initFreshCounter

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

findAllTVars :: Type -> S.Set TName
findAllTVars (TVar t) = S.singleton t
findAllTVars (TFun a r) = S.union (findAllTVars a) (findAllTVars r)
findAllTVars (TList t) = findAllTVars t
findAllTVars _ = S.empty

replaceAllTVars :: UEnv -> Type -> Type
replaceAllTVars uenv (TVar tv) = fromMaybe (TVar tv) (lookupEnv uenv tv)
replaceAllTVars uenv (TFun a r) = TFun (replaceAllTVars uenv a) (replaceAllTVars uenv r)
replaceAllTVars _ t = t

typeVarName :: Type -> Name
typeVarName (TVar n) = n
typeVarName _ = error "Invalid call."

funReturnType :: Type -> Type
funReturnType (TFun _ t) = t
funReturnType _ = error "Invalid call."

emptyTEnv :: TEnv
emptyTEnv = M.empty

emptyUEnv :: UEnv
emptyUEnv = M.empty

lookupEnv :: TEnv -> Name -> Maybe Type
lookupEnv tenv name = M.lookup name tenv

insertEnv :: TEnv -> Name -> Type -> TEnv
insertEnv tenv name t = M.insert name t tenv

check :: TEnv -> UEnv -> Exp -> TypeCheck (TEnv, UEnv, Type)
check tenv uenv e = case e of
  (Lit lit) ->
    case lit of
      IntV _ -> return (tenv, uenv, TInt)
      BoolV _ -> return (tenv, uenv, TBool)
      StringV _ -> return (tenv, uenv, TString)

  (Var v) ->
    case lookupEnv tenv v of
      Just t  -> return (tenv, uenv, t)
      Nothing -> throwE (UnboundVariable v)

  (Let (Var v) val body) -> do
    (tenv', uenv', valT) <- check tenv uenv val
    let tenv'' = insertEnv tenv' v valT
    (_, uenv'', valB) <- check tenv'' uenv' body
    -- Return original tenv because 'v' is no longer in scope.
    return (tenv, uenv'', valB)

  (Let _ _ _) -> throwE (GenericTypeError (Just "Let binding must be a variable"))

  (List elems) ->
    case elems of
      [] -> getFresh >>= (\tv -> return (tenv, uenv, TList tv))
      [el] -> do
        (_, uenv', eT) <- check tenv uenv el
        return (tenv, uenv', TList eT)
      (e1:e2:es) -> do
        (_, uenv', e1T) <- check tenv uenv e1
        (_, uenv'', e2T) <- check tenv uenv' e2
        (uenv''', _) <- unify uenv'' e1T e2T
        check tenv uenv''' (List (e2:es))

  (If p t body) -> do
    (_, uenv', pT) <- check tenv uenv p
    case pT of
      TBool -> do
        (_, uenv'', tT) <- check tenv uenv' t
        (_, uenv''', eT) <- check tenv uenv'' body
        (uenv'''', retT) <- unify uenv''' tT eT
        return (tenv, uenv'''', retT)
      otherT -> throwE $ Mismatch TBool otherT

  (NullaryFun body) -> check tenv uenv body

  (Fun (Var v) body) -> do
    -- Get fresh type variable for argument variable, and bind the arg var to this
    -- type var.
    tv <- getFresh
    let tenv' = insertEnv tenv v tv
    -- Check body
    (tenv'', uenv', bodyT) <- check tenv' uenv body
    -- May have figured out argument's type when body was checked. If not, use the
    -- fresh we got.
    let argT = fromMaybe tv (lookupEnv tenv'' v)
    return (tenv, uenv', TFun (replaceAllTVars uenv' argT) (replaceAllTVars uenv' bodyT))

  (Fun _ _) -> throwE (GenericTypeError (Just "Fun binding must be a variable"))

  (NullaryApp body) -> check tenv uenv body

  (UnaryApp fn arg) -> do
    (_, uenv', fnT) <- check tenv uenv fn
    (_, uenv'', argT) <- check tenv uenv' arg
    case fnT of
      -- If function term is a function, then unify the argument type and return
      -- the return type.
      (TFun fna fnb) -> do
        (uenv''', _) <- unify uenv'' fna argT
        return (tenv, uenv''', replaceAllTVars uenv''' fnb)

      -- If function term is a var, unify that var with what we think the
      -- function's type should be (given the type of the argument).
      (TVar fnVarT) -> do
        fnbT <- getFresh
        (uenv''', fnT') <- unify uenv'' (TVar fnVarT) (TFun argT fnbT)
        -- We partially know the type of the function. Add that to
        -- the tenv so that parent can use it in their type check.
        let tenv' = insertEnv tenv fnVarT fnT'
        return (tenv', uenv''', replaceAllTVars uenv''' (funReturnType fnT'))

      -- Error case: function term isn't a function or variable.
      t -> do
        retT <- getFresh
        throwE (Mismatch (TFun argT retT) t)

  (Def _ _) -> error "TODO"
  (Add _ _) -> error "TODO"

-- | Check unary function call.
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
--     Works: ((\x -> x x) id)
--
-- >>> runCheck (M.fromList [("f",TFun (TVar "a") (TVar "a"))]) emptyUEnv (UnaryApp (Fun (Var "x") (UnaryApp (Var "x") (Var "x"))) (Var "f"))
-- (fromList [("f",(-> a a))],fromList [("x",(-> a a))],(-> (-> a a) (-> a a)))
--

runCheck :: TEnv -> UEnv -> Exp -> (TEnv, UEnv, Type)
runCheck tenv uenv expr =
  case evalState (runExceptT (check tenv uenv expr)) initFreshCounter of
    Left e -> error (show e)
    Right r -> r
