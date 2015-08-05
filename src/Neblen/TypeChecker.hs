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
getFresh :: TypeCheck Type
getFresh = do
  s <- lift get -- Same as: ExceptT (liftM Right get)
  lift $ put s{getFreshCounter = getFreshCounter s + 1}
  return $ TVar (letters !! getFreshCounter s)

-- Mapping of variables (value vars, *not* type variables) to its type.
type TEnv = M.Map Name TypeScheme

-- Type variable.
type TName = String

-- Type variable substitutions. Mapping of type variables to its type.
type Subst = M.Map TName Type

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
  deriving (Eq, Ord) -- Ord for Set functions

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
  deriving (Eq, Ord)

instance Show TypeScheme where
  show (Forall tvs t) = "∀:" ++ show tvs ++ " " ++ show t

toScheme :: Type -> TypeScheme
toScheme = Forall []

-- TODO This is a bad method! Need to merge TypeSchemes instead.
typeSchemeToType :: TypeScheme -> Type
typeSchemeToType (Forall _ t) = t

toTName :: Type -> TName
toTName (TVar t) = t
toTName _ = error "Not TVar!"

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

-- Something is Substitutable if you can apply the given Subst to it, substituting
-- type variables in 't' with its mapping in the Subst.
class Substitutable t where
  -- Substitute free type variables in 't' with mapping found in Subst.
  apply :: Subst -> t -> t

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
  apply s (TVar tv) = fromMaybe (TVar tv) (M.lookup tv s)
  apply s (TFun a r) = TFun (apply s a) (apply s r)
  apply s (TList t) = TList (apply s t)
  apply _ t = t

  tvars (TVar tv) = S.singleton (TVar tv)
  tvars (TFun a r) = S.union (tvars a) (tvars r)
  tvars (TList t) = tvars t
  tvars _ = S.empty

instance Substitutable TypeScheme where
  apply s (Forall tvs t) = Forall tvs (apply s t)
  tvars (Forall tvs _) = S.fromList (map TVar tvs)

-- | Compose two Substs together: apply u1's substitutions over u2's values.
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
-- Below composes the wrong way. The resulting Subst is not useful (as the
-- variables aren't all properly substituted).
--
-- >>> compose (M.fromList [("a", TFun (TVar "b") (TVar "c"))]) (M.fromList [("c", TVar "b")])
-- fromList [("a",(-> b c)),("c",b)]
--
compose :: Subst -> Subst -> Subst
compose u1 u2 = M.union (M.map (apply u1) u2) u1

-- | Compose Substs left-to-right. See 'compose' function for comments.
--
-- >>> composeAll [(M.fromList [("c", TVar "b")]), (M.fromList [("a", TFun (TVar "b") (TVar "c"))])]
-- fromList [("a",(-> b b)),("c",b)]
--
composeAll :: [Subst] -> Subst
composeAll = foldl compose emptySubst

-- | Unify types.
--
unify :: Type -> Type -> TypeCheck Subst
unify TInt TInt = return emptySubst
unify TBool TBool = return emptySubst
unify TString TString = return emptySubst
unify (TVar tv) t2 = unifyTVar (TVar tv) t2
unify t1 (TVar tv) = unifyTVar (TVar tv) t1
unify (TFun a1 r1) (TFun a2 r2) = do
  s' <- unify a1 a2
  s'' <- unify (apply s' r1) (apply s' r2)
  -- We now have a substitution built from both argument and return types. Substitue
  -- any remaining type variables.
  return (composeAll [s'', s'])

-- Assume order implies attempted function call on a non-function.
unify t TFun{} = throwE (FunctionExpected t)
unify t1 t2 = throwE $ Mismatch t1 t2

unifyTVar :: Type -> Type -> TypeCheck Subst
unifyTVar t1@(TVar tv) t2 | t1 == t2 = return emptySubst
                          | occursCheck t1 t2 = throwE $ InfiniteType t1 t2
                          | otherwise = return (M.singleton tv t2)
unifyTVar _ _ = error "Bad call to unifyTVar"

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

emptySubst :: Subst
emptySubst = M.empty

lookupTEnv :: TEnv -> Name -> Maybe TypeScheme
lookupTEnv tenv name = M.lookup name tenv

insertTEnv :: TEnv -> Name -> TypeScheme -> TEnv
insertTEnv tenv name t = M.insert name t tenv

-- | Close over unbounded type variables.
--
-- Should only be used in let statements (for now), as in let-polymorphism.
-- Why don't functions get to generalize?
--
closeOver :: Subst -> Type -> TypeScheme
closeOver s t =
  let ftvs = S.difference (tvars t) (S.fromList (map TVar (M.keys s)))
  in Forall (map toTName (S.elems ftvs)) t

-- | Check type.
--
-- Below is:
--
-- (let [id (fn [x] x)]
--   (let [y (id 3)]
--     (let [z] (id true) 0)))
--
-- >>> runCheck emptyTEnv emptySubst (Let (Var "id") (Fun (Var "x") (Var "x")) (Let (Var "y") (UnaryApp (Var "id") (Lit (IntV 3))) (Let (Var "z") (UnaryApp (Var "id") (Lit (BoolV True))) (Lit (IntV 0)))))
-- (fromList [],fromList [("b",Int),("c",Int),("d",Bool),("e",Bool)],Int)
--
check :: TEnv -> Subst -> Exp -> TypeCheck (TEnv, Subst, Type)
check tenv s e = case e of
  Lit lit ->
    case lit of
      IntV _ -> return (tenv, s, TInt)
      BoolV _ -> return (tenv, s, TBool)
      StringV _ -> return (tenv, s, TString)

  Var v ->
    case lookupTEnv tenv v of
      Just t  -> do
        t' <- freshen (apply s t)
        return (tenv, s, t')
      Nothing -> throwE (UnboundVariable v)

  Let (Var v) val body -> do
    (tenv', s', valT) <- check tenv s val
    let tenv'' = insertTEnv tenv' v (closeOver s' valT) -- let-polymorphism
    (_, s'', valB) <- check tenv'' s' body
    -- Return original tenv because 'v' is no longer in scope.
    return (tenv, composeAll [s'', s', s], valB)

  Let{} -> throwE (GenericTypeError (Just "Let binding must be a variable"))

  List elems ->
    case elems of
      [] -> getFresh >>= (\tv -> return (tenv, s, TList tv))
      [el] -> do
        (_, s1, eT) <- check tenv s el
        return (tenv, composeAll [s1, s], TList (apply s1 eT))
      (e1:e2:es) -> do
        (_, s1, e1T) <- check tenv s e1
        (_, s2, e2T) <- check tenv s1 e2
        s3 <- unify (apply s2 e1T) (apply s2 e2T)
        check tenv (composeAll [s3, s2, s1, s]) (List (e2:es))

  If p t el -> do
    (_, s', pT) <- check tenv s p
    case pT of
      TBool -> do
        (_, s'', tT) <- check tenv s' t
        (_, s''', eT) <- check tenv s'' el
        s'''' <- unify tT eT
        return (tenv, composeAll [s'''', s''', s'', s', s], apply s'''' tT)
      otherT -> throwE $ Mismatch TBool otherT

  NullaryFun body -> check tenv s body

  Fun (Var v) body -> do
    -- Get fresh type variable for argument variable, and bind the arg var to this
    -- type var.
    tv <- getFresh
    let tenv' = insertTEnv tenv v (toScheme tv)
    (tenv'', s', bodyT) <- check tenv' s body
    -- May have figured out argument's type when body was checked. If not, use the
    -- fresh we got.
    -- let argT = fromMaybe (toScheme tv) (lookupTEnv tenv'' v)
    -- TODO don't do typeSchemeToType. We need a way to merge TypeSchemes
    -- together!
    --
    -- return (tenv, s', TFun (apply s' (typeSchemeToType argT)) bodyT)
    -- return (tenv, s', TFun (apply s' tv) (apply s' bodyT))
    return (tenv, s', TFun (apply s' tv) bodyT)

  Fun{} -> throwE (GenericTypeError (Just "Fun binding must be a variable"))

  NullaryApp body -> check tenv s body

  UnaryApp fn arg -> do
    (_, s1, fnT) <- check tenv s fn
    (_, s2, argT) <- check tenv s1 arg
    retT <- getFresh

    s3 <- unify (apply s2 fnT) (apply s2 (TFun argT retT))
    return (tenv, composeAll [s3, s2, s1, s], apply s3 retT)

  Def{} -> error "TODO"
  Add{} -> error "TODO"

-- | Insert fresh variables for universally-quantified types.
--
freshen :: TypeScheme -> TypeCheck Type
freshen ts = liftM snd (freshenWithSubst emptySubst ts)

-- | Insert fresh variables for universally-quantified types.
--
freshenWithSubst :: Subst -> TypeScheme -> TypeCheck (Subst, Type)
freshenWithSubst s (Forall ftvs (TVar tv)) =
  if tv `elem` ftvs
  then case M.lookup tv s of
       Just ftv -> return (s, ftv)
       Nothing -> do
         ftv <- getFresh
         return (M.insert tv ftv s, ftv)
  else return (s, TVar tv)
freshenWithSubst s (Forall ftvs (TFun a r)) = do
  (s1, a1) <- freshenWithSubst s (Forall ftvs a)
  (s2, r1) <- freshenWithSubst s1 (Forall ftvs r)
  return (s2, TFun a1 r1)
freshenWithSubst s (Forall _ t) = return (s, t)

------------------------------------------
-- Helpers to run the monad transformers.
------------------------------------------

runUnify :: TypeCheck Subst -> Either TypeError Subst
runUnify uc = evalState (runExceptT uc) initFreshCounter

runCheck :: TEnv -> Subst -> Exp -> (TEnv, Subst, Type)
runCheck tenv s expr =
  case evalState (runExceptT (check tenv s expr)) initFreshCounter of
    Left e -> error (show e)
    Right r -> r
