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
          | TVec Type
          | TVar TName
  deriving (Eq)

instance Show Type where
  show TInt = "Int"
  show TBool = "Bool"
  show TString = "String"
  show (TFun a r) = "(-> " ++ show a ++ " " ++ show r ++ ")"
  show (TList a) = "List " ++ show a
  show (TVec a) = "Vector " ++ show a
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
  (uenv'', tr) <- unify uenv' r1 r2
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
                                  -- Example: a <==> b.
                                  -- UEnv: {a -> b, b -> a}
                                  -- Returned: a
                                  return (insertEnv (insertEnv uenv tv (TVar tv2)) tv2 (TVar tv), TVar tv)

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
-- Int
--
-- >>> bindFree "x" TInt (TVar "y")
-- y
--
-- >>> bindFree "x" TInt (TFun (TVar "x") (TVar "y"))
-- (-> Int y)
--
-- >>> bindFree "x" TInt (TList (TVar "x"))
-- List Int
--
-- >>> bindFree "x" TInt (TList (TVar "y"))
-- List y
--
-- >>> bindFree "x" TInt (TVec (TVar "x"))
-- Vector Int
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

funReturnType :: Type -> Type
funReturnType (TFun _ t) = t
funReturnType _ = error "Invalid call."

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

-- Check literal.
--
checkLiteral :: TEnv -> UEnv -> Exp -> TypeCheck (TEnv, UEnv, Type)
checkLiteral tenv uenv (Literal (IntV _)) = return (tenv, uenv, TInt)
checkLiteral tenv uenv (Literal (BoolV _)) = return (tenv, uenv, TBool)
checkLiteral tenv uenv (Literal (StringV _)) = return (tenv, uenv, TString)
checkLiteral _ _ _ = throwE emptyGenericTypeError

-- | Check variable.
--
checkVar :: TEnv -> UEnv -> Exp -> TypeCheck (TEnv, UEnv, Type)
checkVar tenv uenv (Var v) =
  case lookupEnv tenv v of
    Just t  -> return (tenv, uenv, t)
    Nothing -> throwE (UnboundVariable v)
checkVar _ _ _ = error "wrong type"

-- | Check let.
--
-- >>> check emptyTEnv emptyUEnv (Let (Var "x") (Literal (IntV 0)) (Var "x"))
-- (fromList [],fromList [],Int)
--
checkLet :: TEnv -> UEnv -> Exp -> TypeCheck (TEnv, UEnv, Type)
checkLet tenv uenv (Let (Var v) val body) = do
  (tenv', uenv', valT) <- checkExp tenv uenv val
  let tenv'' = insertEnv tenv' v valT
  (_, uenv'', valB) <- checkExp tenv'' uenv' body
  -- Return original tenv because 'v' is no longer in scope.
  return (tenv, uenv'', valB)
checkLet _ _ _ = throwE emptyGenericTypeError

-- | Check nullary function.
--
checkNullFun :: TEnv -> UEnv -> Exp -> TypeCheck (TEnv, UEnv, Type)
checkNullFun tenv uenv (NullaryFun body) = checkExp tenv uenv body
checkNullFun _ _ _ = throwE emptyGenericTypeError

-- | Check function.
--
checkFun :: TEnv -> UEnv -> Exp -> TypeCheck (TEnv, UEnv, Type)
checkFun tenv uenv (Function (Var v) body) = do
  -- Get fresh type variable for argument variable, and bind the arg var to this
  -- type var.
  tv <- getFresh
  let tenv' = insertEnv tenv v tv -- TODO: need to insert another mapping for tv?

  -- Check body
  (tenv'', uenv', bodyT) <- checkExp tenv' uenv body

  -- May have figured out argument's type when body was checked. If not, use the
  -- fresh we got.
  let argT = fromMaybe tv (lookupEnv tenv'' v)
  -- let argT = fromMaybe (TVar v) (lookupEnv tenv'' v)

  -- TODO: Do we need to return the updated tenv and uenv? Or the updated ones,
  -- minus the ones that were bound by the function argument.
  --
  -- Can a function body update a variable's type?
  --
  -- (fn [x] (let [z (fn [y] (x 3))] x))
  --
  --     tenv: {}  uenv: {}
  -- - From the outer fn, we getFresh for x.
  --     tenv: {x -> a}  uenv: {}
  -- - In the let binding, we find the inner function
  --     tenv: {x -> a}  uenv: {}
  -- - Infering the function type
  --     tenv: {x -> a, y -> b}  uenv: {a -> (-> Int c)}
  -- - Infered the function type, exiting
  --     tenv: {x -> a}  uenv: {a -> (-> Int c)}
  -- - Exit the let binding, going to the let body
  --     tenv: {x -> a}  uenv: {a -> (-> Int c)}
  -- - In the let body, type check x
  --     tenv: {x -> a}  uenv: {a -> (-> Int c)}
  return (tenv, uenv', TFun (replaceAllTVars uenv' argT) (replaceAllTVars uenv' bodyT))
checkFun _ _ _ = throwE emptyGenericTypeError

-- | Check nullary function call.
--
-- >>> check (M.fromList [("x",TBool)]) emptyUEnv (NullaryCall (Var "x"))
-- (fromList [("x",Bool)],fromList [],Bool)
--
checkNullCall :: TEnv -> UEnv -> Exp -> TypeCheck (TEnv, UEnv, Type)
checkNullCall tenv uenv (NullaryCall (Var v)) = checkExp tenv uenv (Var v)
checkNullCall tenv uenv (NullaryCall (NullaryFun body)) = checkExp tenv uenv (NullaryFun body)
checkNullCall _ _ _ = throwE emptyGenericTypeError

-- | Check unary function call.
--
-- =======convert to test pole========
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
-- >>> check (M.fromList [("f",TFun (TVar "a") (TVar "a"))]) emptyUEnv (UnaryCall (Function (Var "x") (UnaryCall (Var "x") (Var "x"))) (Var "f"))
-- (fromList [("f",(-> a a))],fromList [("x",(-> a a))],(-> (-> a a) (-> a a)))
--
checkUnaryCall :: TEnv -> UEnv -> Exp -> TypeCheck (TEnv, UEnv, Type)
checkUnaryCall tenv uenv (UnaryCall fn arg) = do
  (_, uenv', fnT) <- checkExp tenv uenv fn
  -- TODO merge the first two case statements?
  case fnT of
    (TFun fna fnb) -> do
      (_, uenv'', argT) <- checkExp tenv uenv' arg
      (uenv''', _) <- unify uenv'' fna argT
      return (tenv, uenv''', replaceAllTVars uenv''' fnb)

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
  -- , testCase "checkfun: (fn [a] (fn [x] (a x))) : (-> (-> a b) b)" $
  --   run (checkExp emptyTEnv emptyUEnv (Function (Var "a") (Function (Var "x") (UnaryCall (Var "a") (Var "x")))))
  --   @?= (M.fromList [],M.fromList [],TFun (TFun (TVar "a") (TVar "b")) (TVar "b"))
  --
    (TVar fnVarT) -> do
      (_, uenv'', argT) <- checkExp tenv uenv' arg
      fnbT <- getFresh
      (uenv''', fnT') <- unify uenv'' (TVar fnVarT) (TFun argT fnbT)
      -- We partially know the type of the function. Add that to
      -- the tenv so that parent can use it in their type check.
      let tenv' = insertEnv tenv fnVarT fnT'
      return (tenv', uenv''', replaceAllTVars uenv''' (funReturnType fnT'))
    t -> do
      -- Find argument type for better error message.
      (_, _, argT) <- checkExp tenv uenv' arg
      retT <- getFresh
      throwE (Mismatch (TFun argT retT) t)
checkUnaryCall _ _ _ = throwE emptyGenericTypeError

-- | Check list.
--
-- >>> check emptyTEnv emptyUEnv (List [Literal (IntV 0)])
-- (fromList [],fromList [],List Int)
--
-- Below is: (list 0 ((fn [x] x) 0))
-- >>> check emptyTEnv emptyUEnv (List [Literal (IntV 0),UnaryCall (Function (Var "x") (Var "x")) (Literal (IntV 0))])
-- (fromList [],fromList [("x",Int)],List Int)
--
-- >>> check emptyTEnv emptyUEnv (List [Literal (IntV 0),Literal (BoolV True)])
-- *** Exception: list type mismatch: Int and Bool found
--
-- >>> check emptyTEnv emptyUEnv (List [])
-- (fromList [],fromList [],List a)
--
checkList :: TEnv -> UEnv -> Exp -> TypeCheck (TEnv, UEnv, Type)
checkList tenv uenv (List []) = do
  tv <- getFresh
  return (tenv, uenv, TList tv)
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
checkVector :: TEnv -> UEnv -> Exp -> TypeCheck (TEnv, UEnv, Type)
checkVector = checkList

-- | Check if.
--
-- Below is: (if ((fn [x] true) 0) "truth" "false") : String
-- >>> check emptyTEnv emptyUEnv (If (UnaryCall (Function (Var "x") (Literal (BoolV True))) (Literal (IntV 0))) (Literal (StringV "then clause")) (Literal (StringV "else clause")))
-- (fromList [],fromList [("x",Int)],String)
--
-- Below is: (if false 0 false)
-- >>> check emptyTEnv emptyUEnv (If (Literal (BoolV False)) (Literal (IntV 0)) (Literal (BoolV False)))
-- *** Exception: then and else clause type mismatch: Int and Bool found
--
checkIf :: TEnv -> UEnv -> Exp -> TypeCheck (TEnv, UEnv, Type)
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
checkExp :: TEnv -> UEnv -> Exp -> TypeCheck (TEnv, UEnv, Type)
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
