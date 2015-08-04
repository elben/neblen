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
  deriving (Eq, Ord)

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
               | FunctionExpected Type
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
  show (FunctionExpected t) = "type mismatch: expecting function but got " ++ show t
  show (UnboundVariable n) = "unbound variable " ++ n
  show (InfiniteType tvar t) = "cannot resolve infinite type " ++ show tvar ++ " in " ++ show t
  show (GenericTypeError (Just msg)) = "type error: " ++ msg
  show (GenericTypeError Nothing) = "type error"

newtype FreshCounter = FreshCounter { getFreshCounter :: Int }

initFreshCounter :: FreshCounter
initFreshCounter = FreshCounter { getFreshCounter = 0 }

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

-- Something is Subsitutable if you can apply the given UEnv to it, substituting
-- type variables in 't' with its mapping in the UEnv.
class Substitutable t where
  -- Substitute free type variables in 't' with mapping found in UEnv.
  apply :: UEnv -> t -> t

  -- Returns list of type variables found in 't'.
  tvars :: t -> S.Set Type

instance Substitutable a => Substitutable [a] where
  apply s = map (apply s)

  tvars = foldl (\tvs t -> S.union tvs (tvars t)) S.empty

-- | Replace all the type variables with its mapping.
--
-- >>> apply (M.fromList [("a",TVar "x"),("b",TInt)]) (TFun (TVar "a") (TFun (TVar "b") TString))
-- (-> x (-> Int String))
--
-- >>> tvars (TFun (TVar "a") (TFun (TVar "b") TString))
-- fromList [a,b]
--
instance Substitutable Type where
  apply uenv (TVar tv) = fromMaybe (TVar tv) (lookupEnv uenv tv)
  apply uenv (TFun a r) = TFun (apply uenv a) (apply uenv r)
  apply uenv (TList t) = TList (apply uenv t)
  apply _ t = t

  tvars (TVar tv) = S.singleton (TVar tv)
  tvars (TFun a r) = S.union (tvars a) (tvars r)
  tvars (TList t) = tvars t
  tvars _ = S.empty

-- | Compose two UEnvs together: apply u1's substitutions over u2's values.
-- Example:
--
--   u1: {c -> b}
--   u2: {a -> (-> b c)}
--   (compose u1 u2): {a -> (-> b b), c -> b}
--
-- Note that order matters. u1's mapping is applied to u2's mapping, but
-- not vice-versa.
--
-- >>> compose (M.fromList [("c", TVar "b")]) (M.fromList [("a", TFun (TVar "b") (TVar "c"))])
-- fromList [("a",(-> b b)),("c",b)]
--
-- Below composes the wrong way. The resulting uenv is not useful (as the
-- variables aren't all properly substituted).
--
-- >>> compose (M.fromList [("a", TFun (TVar "b") (TVar "c"))]) (M.fromList [("c", TVar "b")])
-- fromList [("a",(-> b c)),("c",b)]
--
compose :: UEnv -> UEnv -> UEnv
compose u1 u2 = M.union (M.map (apply u1) u2) u1

-- | Compose UEnvs left-to-right. See 'compose' function for comments.
--
-- >>> composeAll [(M.fromList [("c", TVar "b")]), (M.fromList [("a", TFun (TVar "b") (TVar "c"))])]
-- fromList [("a",(-> b b)),("c",b)]
--
composeAll :: [UEnv] -> UEnv
composeAll = foldl compose emptyUEnv

-- | Unify types.
--
-- TODO: what if this just returns the UEnv, then users "apply" (see
-- write-you-a-haskell chatper 7 poly example) the new uenv with their old
-- stuff. This may simplify a lot of stuff!
--
unify :: UEnv -> Type -> Type -> TypeCheck (UEnv, Type)
unify uenv TInt TInt = return (uenv, TInt)
unify uenv TBool TBool = return (uenv, TBool)
unify uenv TString TString = return (uenv, TString)
unify uenv (TVar tv) t2 = unifyTVar uenv (TVar tv) t2
unify uenv t1 (TVar tv) = unifyTVar uenv (TVar tv) t1
unify uenv (TFun a1 r1) (TFun a2 r2) = do
  (uenv', ta) <- unify uenv a1 a2
  (uenv'', tr) <- unify uenv' (apply uenv' r1) (apply uenv' r2)
  -- We now have a u-env built from both argument and return types. Substitue
  -- any remaining type variables.
  return (composeAll [uenv'', uenv', uenv], TFun (apply uenv'' ta) (apply uenv'' tr))

-- Assume order implies attempted function call on a non-function.
unify _ t TFun{} = throwE (FunctionExpected t)
unify _ t1 t2 = throwE $ Mismatch t1 t2

unifyTVar :: UEnv -> Type -> Type -> TypeCheck (UEnv, Type)
unifyTVar uenv tvar@(TVar tv) t2 =
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

                              | occursCheck tvar t2 -> throwE $ InfiniteType (TVar tv) t2
                              | otherwise ->
                                  -- Unify free variables together.
                                  --
                                  -- Example: a <==> b
                                  -- UEnv: {b -> a}
                                  -- Returned: a
                                  return (insertEnv uenv tv2 (TVar tv), TVar tv)

                      -- Resolved RHS. In the example above, b (RHS) is resolved to
                      -- Int. So we need to update uenv to {b -> Int, a -> Int}
                      Just t2' -> let uenv' = insertEnv uenv tv2 t2'
                                      uenv'' = insertEnv uenv' tv t2'
                                  in unify (composeAll [uenv'', uenv', uenv]) (TVar tv2) t2'
        _      -> if occursCheck tvar t2
                  then throwE $ InfiniteType (TVar tv) t2
                  else let uenv' = insertEnv uenv tv t2
                       in unify (composeAll [uenv', uenv]) (TVar tv) t2
    Just t1 ->
      unify uenv t1 t2
unifyTVar _ _ _ = error "bad call to unifyTVar"

-- | This occurs check asserts that if we are applying a substitution of variable
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
-- >>> occursCheck (TVar "a") (TFun TInt (TVar "a"))
-- True
--
-- >>> occursCheck (TVar "a") (TFun TInt (TVar "b"))
-- False
--
occursCheck :: Type -> Type -> Bool
occursCheck tv t = S.member tv (tvars t)

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
  Lit lit ->
    case lit of
      IntV _ -> return (tenv, uenv, TInt)
      BoolV _ -> return (tenv, uenv, TBool)
      StringV _ -> return (tenv, uenv, TString)

  Var v ->
    case lookupEnv tenv v of
      Just t  -> return (tenv, uenv, t)
      Nothing -> throwE (UnboundVariable v)

  Let (Var v) val body -> do
    (tenv', uenv', valT) <- check tenv uenv val
    let tenv'' = insertEnv tenv' v valT
    (_, uenv'', valB) <- check tenv'' uenv' body
    -- Return original tenv because 'v' is no longer in scope.
    return (tenv, composeAll [uenv'', uenv', uenv], valB)

  Let{} -> throwE (GenericTypeError (Just "Let binding must be a variable"))

  List elems ->
    case elems of
      [] -> getFresh >>= (\tv -> return (tenv, uenv, TList tv))
      [el] -> do
        (_, uenv', eT) <- check tenv uenv el
        return (tenv, composeAll [uenv', uenv], TList eT)
      (e1:e2:es) -> do
        (_, uenv', e1T) <- check tenv uenv e1
        (_, uenv'', e2T) <- check tenv uenv' e2
        (uenv''', _) <- unify uenv'' e1T e2T
        check tenv (composeAll [uenv''', uenv'', uenv', uenv]) (List (e2:es))

  If p t body -> do
    (_, uenv', pT) <- check tenv uenv p
    case pT of
      TBool -> do
        (_, uenv'', tT) <- check tenv uenv' t
        (_, uenv''', eT) <- check tenv uenv'' body
        (uenv'''', retT) <- unify uenv''' tT eT
        return (tenv, composeAll [uenv'''', uenv''', uenv'', uenv', uenv], retT)
      otherT -> throwE $ Mismatch TBool otherT

  NullaryFun body -> check tenv uenv body

  Fun (Var v) body -> do
    -- Get fresh type variable for argument variable, and bind the arg var to this
    -- type var.
    tv <- getFresh
    let tenv' = insertEnv tenv v tv
    -- Check body
    (tenv'', uenv', bodyT) <- check tenv' uenv body
    -- May have figured out argument's type when body was checked. If not, use the
    -- fresh we got.
    let argT = fromMaybe tv (lookupEnv tenv'' v)
    return (tenv, uenv', TFun (apply uenv' argT) (apply uenv' bodyT))

  Fun{} -> throwE (GenericTypeError (Just "Fun binding must be a variable"))

  NullaryApp body -> check tenv uenv body

  UnaryApp fn arg -> do
    (_, uenv', fnT) <- check tenv uenv fn
    (_, uenv'', argT) <- check tenv uenv' arg
    retT <- getFresh
    (uenv''', _) <- unify uenv'' fnT (TFun argT retT)
    return (tenv, uenv''', apply uenv''' retT)

  Def{} -> error "TODO"
  Add{} -> error "TODO"

------------------------------------------
-- Helpers to run the monad transformers.
------------------------------------------

runUnify :: TypeCheck (UEnv, Type) -> Either TypeError (UEnv, Type)
runUnify uc = evalState (runExceptT uc) initFreshCounter

runCheck :: TEnv -> UEnv -> Exp -> (TEnv, UEnv, Type)
runCheck tenv uenv expr =
  case evalState (runExceptT (check tenv uenv expr)) initFreshCounter of
    Left e -> error (show e)
    Right r -> r
