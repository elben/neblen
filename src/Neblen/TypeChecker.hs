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

import Debug.Trace

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
  deriving (Eq, Ord)
  -- ^ Ord for Set functions.

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
toScheme t = Forall [] t

-- TODO This is a bad method! Need to merge TypeSchemes instead.
typeSchemeToType :: TypeScheme -> Type
typeSchemeToType (Forall _ t) = t
typeSchemeToType _ = error "Error!"

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
-- TODO: what if this just returns the Subst, then users "apply" (see
-- write-you-a-haskell chatper 7 poly example) the new subst with their old
-- stuff. This may simplify a lot of stuff!
--
unify :: Subst -> Type -> Type -> TypeCheck Subst
unify s TInt TInt = return emptySubst
unify s TBool TBool = return emptySubst
unify s TString TString = return emptySubst
unify s (TVar tv) t2 = unifyTVar s (TVar tv) t2
unify s t1 (TVar tv) = unifyTVar s (TVar tv) t1
unify s (TFun a1 r1) (TFun a2 r2) = do
  s' <- unify s a1 a2
  s'' <- unify s' (apply s' r1) (apply s' r2)
  -- We now have a substitution built from both argument and return types. Substitue
  -- any remaining type variables.
  return (composeAll [s'', s', s])

-- Assume order implies attempted function call on a non-function.
unify _ t TFun{} = throwE (FunctionExpected t)
unify _ t1 t2 = throwE $ Mismatch t1 t2

unifyTVar :: Subst -> Type -> Type -> TypeCheck Subst
unifyTVar s tvar@(TVar tv) t2 =
  case M.lookup tv s of
    Nothing ->
      case t2 of
        -- If both sides are free, then see if we can resolve the RHS with the
        -- subst. For example, in (-> a a) (-> a b) with subst {b -> Int}, the
        -- LHS failed to resolve, so we try the RHS.
        TVar tv2 -> case M.lookup tv2 s of
                      -- RHS is still free.
                      Nothing | tv == tv2  ->
                                  -- Same free variable: a <==> a
                                  --
                                  -- Return existing subst, since we learned
                                  -- nothing new.
                                  return s

                              | occursCheck tvar t2 -> throwE $ InfiniteType (TVar tv) t2
                              | otherwise ->
                                  -- Unify free variables together.
                                  --
                                  -- Example: a <==> b
                                  -- Subst: {b -> a}
                                  -- Returned: a
                                  --
                                  return (M.insert tv2 (TVar tv) s)

                      -- Resolved RHS. In the example above, b (RHS) is resolved to
                      -- Int. So we need to update s to {b -> Int, a -> Int}
                      Just t2' -> let s' = M.insert tv2 t2' s
                                      s'' = M.insert tv t2' s'
                                  in unify (composeAll [s'', s', s]) (TVar tv2) t2'
        _      -> if occursCheck tvar t2
                  then throwE $ InfiniteType (TVar tv) t2
                  else let s' = M.insert tv t2 s
                       in unify (composeAll [s', s]) (TVar tv) t2
    Just t1 ->
      unify s t1 t2
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

emptySubst :: Subst
emptySubst = M.empty

lookupTEnv :: TEnv -> Name -> Maybe TypeScheme
lookupTEnv tenv name = M.lookup name tenv

insertEnv :: TEnv -> Name -> TypeScheme -> TEnv
insertEnv tenv name t = M.insert name t tenv

closeOver :: Subst -> Type -> TypeScheme
closeOver s t =
  let ftvs = S.difference (tvars t) (S.fromList (map TVar (M.keys s)))
  in Forall (map toTName (S.elems ftvs)) t

-- | Check type.
--
-- (let [id (fn [x] x)]
--   (let [y (id 3)]
--     (let [z] (id true) 0)))
--
-- >>> runCheck emptyTEnv emptySubst (Let (Var "id") (Fun (Var "x") (Var "x")) (Let (Var "y") (UnaryApp (Var "id") (Lit (IntV 3))) (Let (Var "z") (UnaryApp (Var "id") (Lit (BoolV True))) (Lit (IntV 0)))))
--
--
-- >>> runCheck emptyTEnv emptySubst (Let (Var "id") (Fun (Var "x") (Var "x")) (Let (Var "y") (UnaryApp (Var "id") (Lit (IntV 3))) (Let (Var "z") (UnaryApp (Var "id") (Lit (BoolV True))) (Let (Var "o") (UnaryApp (Var "id") (Lit (StringV "hello"))) (Lit (IntV 0))))))
--
check :: TEnv -> Subst -> Exp -> TypeCheck (TEnv, Subst, TypeScheme)
check tenv s e = case (trace ("tenv: " ++ show tenv ++ "\tsubst: " ++ show s) e) of
  Lit lit ->
    case lit of
      IntV _ -> return (tenv, s, toScheme TInt)
      BoolV _ -> return (tenv, s, toScheme TBool)
      StringV _ -> return (tenv, s, toScheme TString)

  Var v ->
    case lookupTEnv tenv v of
      Just t  -> return (tenv, s, t)
      Nothing -> throwE (UnboundVariable v)

  Let (Var v) val body -> do
    (tenv', s', valT) <- check tenv s val
    let tenv'' = insertEnv tenv' v valT
    (_, s'', valB) <- check tenv'' s' body
    -- Return original tenv because 'v' is no longer in scope.
    return (tenv, composeAll [s'', s', s], valB)

  Let{} -> throwE (GenericTypeError (Just "Let binding must be a variable"))

  -- List elems ->
  --   case elems of
  --     [] -> getFresh >>= (\tv -> return (tenv, s, toScheme $ TList tv))
  --     [el] -> do
  --       (_, s', eT) <- check tenv s el
  --       return (tenv, composeAll [s', s], closeOver s' (TList eT))
  --     (e1:e2:es) -> do
  --       (_, s', e1T) <- check tenv s e1
  --       (_, s'', e2T) <- check tenv s' e2
  --       (s''', _) <- unify s'' e1T e2T
  --       check tenv (composeAll [s''', s'', s', s]) (List (e2:es))

  -- If p t body -> do
  --   (_, s', pT) <- check tenv s p
  --   case pT of
  --     TBool -> do
  --       (_, s'', tT) <- check tenv s' t
  --       (_, s''', eT) <- check tenv s'' body
  --       (s'''', retT) <- unify s''' tT eT
  --       return (tenv, composeAll [s'''', s''', s'', s', s], retT)
  --     otherT -> throwE $ Mismatch TBool otherT

  NullaryFun body -> check tenv s body

  Fun (Var v) body -> do
    -- Get fresh type variable for argument variable, and bind the arg var to this
    -- type var.
    tv <- getFresh
    let tenv' = insertEnv tenv v (closeOver s tv)
    -- Check body
    (tenv'', s', bodyT) <- check tenv' s body
    -- May have figured out argument's type when body was checked. If not, use the
    -- fresh we got.
    let argT = fromMaybe (closeOver s' tv) (lookupTEnv tenv'' v)
    -- TODO don't do typeSchemeToType. We need a way to merge TypeSchemes
    -- together as we close over them!
    --
    -- let ts1 = closeOver s' argT
    -- let ts2 = closeOver s' argT
    --
    return (tenv, s', closeOver s' (TFun (apply s' (typeSchemeToType argT)) (apply s' (typeSchemeToType bodyT))))

  Fun{} -> throwE (GenericTypeError (Just "Fun binding must be a variable"))

  NullaryApp body -> check tenv s body

  UnaryApp fn arg -> do
    (_, s', fnT) <- check tenv s fn
    (_, s'', argT) <- check tenv s' arg
    retT <- getFresh

    fnT' <- freshen fnT
    argT' <- freshen argT

    s''' <- unify s'' fnT' (TFun argT' retT)
    return (tenv, composeAll [s''', s'', s', s], apply s''' (closeOver s retT))

  Def{} -> error "TODO"
  Add{} -> error "TODO"

-- | Insert fresh variables for universally-quantified types.
--
freshen :: TypeScheme -> TypeCheck Type
freshen ts = freshenWithSubst emptySubst ts >>= (\a -> return $ snd a)

-- | Insert fresh variables for universally-quantified types.
--
freshenWithSubst :: Subst -> TypeScheme -> TypeCheck (Subst, Type)
freshenWithSubst s (Forall ftvs (TVar tv)) =
  if tv `elem` ftvs
  then case M.lookup tv s of
       Just ftv -> return $ (s, ftv)
       Nothing -> do
         ftv <- getFresh
         return (M.insert tv ftv s, ftv)
  else return (s, TVar tv)
freshenWithSubst s (Forall ftvs (TFun a r)) = do
  (s1, a1) <- freshenWithSubst s (Forall ftvs a)
  (s1, r1) <- freshenWithSubst s1 (Forall ftvs r)
  return (s1, TFun a1 r1)
freshenWithSubst s (Forall ftvs t) = return (s, t)

------------------------------------------
-- Helpers to run the monad transformers.
------------------------------------------

runUnify :: TypeCheck (Subst, Type) -> Either TypeError (Subst, Type)
runUnify uc = evalState (runExceptT uc) initFreshCounter

runCheck :: TEnv -> Subst -> Exp -> (TEnv, Subst, TypeScheme)
runCheck tenv s expr =
  case evalState (runExceptT (check tenv s expr)) initFreshCounter of
    Left e -> error (show e)
    Right r -> r
