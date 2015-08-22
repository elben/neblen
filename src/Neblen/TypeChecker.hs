module Neblen.TypeChecker where

import Neblen.Data
import Neblen.Utils
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

-- = How the type checker works
--
-- At a high level, the type checker is similar to an interpreter: you go
-- through the expression tree and calculate a type for each expression.
--
-- The environment of the type check includes the variable context and the type
-- variable context:
--
--   * 'TEnv' - The mapping of regular variables to its type.
--   * 'Subst' - Short for "substitution," this maps /type/ variables to its
--     type (which may be other type variables).
--
-- As the checker traverses the expression, only the @Subst@ context needs to be
-- threaded in-and-out. That is, a child expression may discover something about
-- a type that the parent expression needs to know about. For example:
--
--   @
--     (fn [x] (x true))
--   @
--
-- When checking this expression, we encounter the function and give @x@ the
-- fresh type variable @a@.
--
-- Then we go into the body of the function, @(x true)@, and discover that @x :
-- a@ must be a function @(-> Bool b)@. This is in the 'Subst' context inside of
-- @(x true)@. Finally, we pop back up to the top expression, and 'apply' this
-- new knowledge back into the original @a@, and substitute @a@ with @(-> Bool
-- b)@.
--
-- == Unification
--
-- == Type schemes and polymorphism
--
-- The 'TEnv' data type actually maps to a 'TypeScheme'. Type schemes help us
-- deal with polymorphic types, especially in let-polymoprhism. For example:
--
--   @
--     (let [id (fn [x] x)
--           u (id 3)
--           v (id true)]
--       ...)
--   @
--
-- In this example, the function @id@ should work for both @u@ and @v@, even
-- though @id@ is instantiated twice with different types. To do this, we need
-- @id@ to be typed as @id : ∀a. (-> a a)@. When the type checker reaches the
-- @u@ binding, it should instantiate new type variables for @id@ (say it
-- chooses the type variable @b@). Then the function application @(id 3)@ is
-- type-checked, we get that instance of @id : (-> Int Int)@. When we
-- check @v@, we get a /new/ @id : (-> Bool Bool)@.
--
-- To simplify our code, we allow type schemes over types without any free type
-- variables: @∀. Int@.
--
-- == The TypeCheck data type
--
-- During our type checking run, we need some stuff:
--
--   * A way to exit the computation if a type error is encountered.
--   * A way to fresh (guaranteed to be unused) type variables (essentially,
--     some stateful counter).
--   * Carry around other contexts like 'Subst'
--
-- To facilitate this, we use a monad transformer stack of 'ExceptT' + 'State'.
-- 'ExceptT' gives us type errors (left) and no type error (right). 'State'
-- gives us an incrementable counter to get fresh type variables from.

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Map.Strict as M

------------------------------------------
-- Data types
------------------------------------------

-- Monad transformer stack for TypeCheck:
--
--   State (fresh variable counter)
--     ExceptT (TypeError)
--       a
--
type TypeCheck a = ExceptT TypeError (State FreshCounter) a

-- Mapping of variables (value vars, *not* type variables) to its type.
type TEnv = M.Map Name TypeScheme

-- Type variable substitutions. Mapping of type variables to its type.
type Subst = M.Map TName Type

-- Type schemes is a way to allow let-polymorphism (ML-style polymorphism):
-- functions can be instantiated with different types in the same body. See
-- Pierce 22.7 Let-Polymorphism (pg 331).
data TypeScheme = Forall [TName] Type
  deriving (Eq, Ord)

data TypeError = Mismatch Type Type
               | FunctionExpected Type
               | UnboundVariable Name
               | InfiniteType Type Type -- InfiniteType TVar Type
               | GenericTypeError (Maybe String)
  deriving (Eq)

newtype FreshCounter = FreshCounter { getFreshCounter :: Int }

-- Something is Substitutable if you can apply the given Subst to it, substituting
-- type variables in 't' with its mapping in the Subst.
class Substitutable t where
  -- Substitute free type variables in 't' with mapping found in Subst.
  apply :: Subst -> t -> t

  -- Returns list of free type variables found in 't'.
  ftvs :: t -> S.Set Type

instance Substitutable a => Substitutable [a] where
  apply s = map (apply s)

  ftvs = foldl (\tvs t -> S.union tvs (ftvs t)) S.empty

-- | Replace all the type variables with its mapping.
--
-- >>> apply (M.fromList [("a",TVar "x"),("b",TInt)]) (TFun [TVar "a",TVar "b",TString])
-- (-> x Int String)
--
-- >>> ftvs (TFun [TVar "a",TVar"b",TString])
-- fromList [a,b]
--
instance Substitutable Type where
  apply s (TVar tv) = fromMaybe (TVar tv) (M.lookup tv s)
  apply s (TFun vs) = TFun (map (apply s) vs)
  apply s (TList t) = TList (apply s t)
  apply _ t = t

  ftvs (TVar tv) = S.singleton (TVar tv)
  ftvs (TFun vs) = foldl (\s v -> S.union s (ftvs v)) S.empty vs
  ftvs (TList t) = ftvs t
  ftvs _ = S.empty

instance Substitutable TypeScheme where
  apply s (Forall tvs t) = Forall tvs (apply s t)
  ftvs (Forall tvs _) = S.fromList (map TVar tvs)

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
-- >>> compose (M.fromList [("c", TVar "b")]) (M.fromList [("a", TFun [TVar "b",TVar "c"])])
-- fromList [("a",(-> b b)),("c",b)]
--
-- Below composes the wrong way. The resulting Subst is not useful (as the
-- variables aren't all properly substituted).
--
-- >>> compose (M.fromList [("a", TFun [TVar "b",TVar "c"])]) (M.fromList [("c", TVar "b")])
-- fromList [("a",(-> b c)),("c",b)]
--
compose :: Subst -> Subst -> Subst
compose u1 u2 = M.union (M.map (apply u1) u2) u1

-- | Compose Substs left-to-right. See 'compose' function for comments.
--
-- >>> composeAll [(M.fromList [("c", TVar "b")]), (M.fromList [("a", TFun [TVar "b",TVar "c"])])]
-- fromList [("a",(-> b b)),("c",b)]
--
composeAll :: [Subst] -> Subst
composeAll = foldl compose emptySubst

-- | Unify types.
--
-- Unification attempts to create a new Subst universe with new mappings of type
-- variables to types. A simple case:
--
--   @a <==> Int@
--
-- When trying to unify @a@ with @Int@, we can simply map @a => Int@.
--
unify :: Type -> Type -> TypeCheck Subst
unify TInt TInt = return emptySubst
unify TBool TBool = return emptySubst
unify TString TString = return emptySubst
unify (TVar tv) t2 = unifyTVar (TVar tv) t2
unify t1 (TVar tv) = unifyTVar (TVar tv) t1

-- Unify functions.
--
-- How functions of differing arity are unified:
--
--   * Unify the shared arguments.
--   * If un-even remaining arguments, unify remaining as functions again.
--   * If one of the sides has only one argument left, that argument must be the
--     return type. Unify that return type with the rest.
--
-- Example:
--
--   1. @(-> Int a b c) <==> (-> d e)@
--   2. @Int@ and @d@ are unified.
--   3. @(-> a b c) <==> (-> e)@
--   4. @(-> a b c)@ and @e@ are unified.
--   5. Unified to @(-> Int (-> a b c))@
--
unify (TFun [l]) (TFun [r]) = unify l r
unify lhs@(TFun _) (TFun [r]) = unify lhs r
unify (TFun [l]) rhs@(TFun _) = unify l rhs
unify (TFun lhs) (TFun rhs) =
  let numArgs    = min (length lhs) (length rhs) - 1
      sharedArgs = L.zip (take numArgs lhs) (take numArgs rhs)
      restL      = drop numArgs lhs
      restR      = drop numArgs rhs
  in do
    -- Unify the arguments shared between lhs and rhs
    s <- foldl
           (\sumSubst (l,r) -> do
             s <- sumSubst
             s1 <- unify (apply s l) (apply s r)
             return $ s1 `compose` s)
           (return emptySubst) sharedArgs

    -- Unify remaining args (including return value)
    s' <- unify (TFun (apply s restL)) (TFun (apply s restR))
    return (s' `compose` s)

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
-- >>> occursCheck (TVar "a") (TFun [TInt,TVar "a"])
-- True
--
-- >>> occursCheck (TVar "a") (TFun [TInt,TVar "b"])
-- False
--
occursCheck :: Type -> Type -> Bool
occursCheck tv t = S.member tv (ftvs t)

-- | Find non-universally quantified type variables in the type scheme.
--
-- >>> nonFree (Forall ["x"] (TVar "x"))
-- fromList []
--
-- >>> nonFree (Forall [] (TVar "x"))
-- fromList [x]
--
-- >>> nonFree (Forall ["x"] (TFun [TVar "x",TVar "y",TVar "z"]))
-- fromList [y,z]
--
nonFree :: TypeScheme -> S.Set Type
nonFree (Forall tvs t) =
  let ftv' = ftvs t
  in ftv' `S.difference` S.fromList (map TVar tvs)

-- | Find non-universally quantified type variables in the type schemes.
--
-- >>> nonFrees [(Forall [] (TVar "a")), (Forall ["x"] (TFun [TVar "x",TVar "y",TVar "z"]))]
-- fromList [a,y,z]
--
nonFrees :: [TypeScheme] -> S.Set Type
nonFrees = foldl (\s ts -> s `S.union` nonFree ts) S.empty

-- | Generalize unbounded type variables into a for-all type scheme.
--
-- >>> generalize (M.fromList [("foo",Forall [] (TVar "a"))]) (M.fromList [("b",TInt)]) (TFun [TVar "a",TVar "b",TVar "c"])
-- ∀:["c"] (-> a b c)
--
generalize :: TEnv -> Subst -> Type -> TypeScheme
generalize tenv s t =
  let bounds = nonFrees (M.elems tenv)
      ftv = (ftvs t `S.difference` S.fromList (map TVar (M.keys s)) `S.difference` bounds)
  in Forall (map toTName (S.elems ftv)) t

-- | Check type and re-order type variables.
--
-- (fn [f] (fn [x] (f x))) : (-> (-> a b) (-> a b))
-- >>> runWithFreshCounter (checkType (Fun [Var "f"] (Fun [Var "x"] (UnaryApp (Var "f") (Var "x")))))
-- Right (-> (-> a b) (-> a b))
--
checkType :: Exp -> TypeCheck Type
checkType e = do
  (_, t) <- check defaultTEnv emptySubst e
  case evalState (runExceptT (reorderTVars t)) initFreshCounter of
    Right t' -> return t'
    Left err -> throwE err

checkTypeWith :: Exp -> TEnv -> TypeCheck Type
checkTypeWith e tenv = do
  (_, t) <- check (M.union defaultTEnv tenv) emptySubst e
  case evalState (runExceptT (reorderTVars t)) initFreshCounter of
    Right t' -> return t'
    Left err -> throwE err

-- | Check type.
--
-- Below is:
--
-- (let [id (fn [x] x)]
--   (let [y (id 3)]
--     (let [z (id true)] z)))
--
-- >>> runCheck emptyTEnv emptySubst (Let (Var "id") (Fun [Var "x"] (Var "x")) (Let (Var "y") (UnaryApp (Var "id") (Lit (IntV 3))) (Let (Var "z") (UnaryApp (Var "id") (Lit (BoolV True))) (Var "z"))))
-- (fromList [("b",Int),("c",Int),("d",Bool),("e",Bool)],Bool)
--
check :: TEnv -> Subst -> Exp -> TypeCheck (Subst, Type)
check tenv s e = case e of
  Lit lit ->
    case lit of
      IntV _ -> return (s, TInt)
      BoolV _ -> return (s, TBool)
      StringV _ -> return (s, TString)

  Var v ->
    case lookupTEnv tenv v of
      Just t  -> do
        t' <- freshen (apply s t)
        return (s, t')
      Nothing -> throwE (UnboundVariable v)

  Let (Var v) rhs body -> do
    (s', rhsT) <- check tenv s rhs
    let tenv' = insertTEnv tenv v (generalize tenv s' rhsT) -- let-polymorphism
    (s'', bodyT) <- check tenv' s' body
    -- Return original tenv because 'v' is no longer in scope.
    return (composeAll [s'', s', s], bodyT)

  Let{} -> throwE (GenericTypeError (Just "Let binding must be a variable"))

  List elems ->
    case elems of
      [] -> getFresh >>= (\tv -> return (s, TList tv))
      [el] -> do
        (s1, eT) <- check tenv s el
        return (composeAll [s1, s], TList (apply s1 eT))
      (e1:e2:es) -> do
        (s1, e1T) <- check tenv s e1
        (s2, e2T) <- check tenv s1 e2
        s3 <- unify (apply s2 e1T) (apply s2 e2T)
        check tenv (composeAll [s3, s2, s1, s]) (List (e2:es))

  If p t el -> do
    (s', pT) <- check tenv s p
    case pT of
      TBool -> do
        (s'', tT) <- check tenv s' t
        (s''', eT) <- check tenv s'' el
        s'''' <- unify tT eT
        return (composeAll [s'''', s''', s'', s', s], apply s'''' tT)
      otherT -> throwE $ Mismatch TBool otherT

  -- Function may have no arguments (e.g. have type (-> a)), which is a way of
  -- doing "lazy" evaluation.
  Fun vs body ->
    if not (isListOfVars vs)
      then throwE (GenericTypeError (Just ("Ill-defined function: " ++ toLisp e)))
    else do
     -- Get fresh type variables for every argument variable.
     tvs <- mapM (const getFresh) vs

     -- Zip the argument variables with its corresponding (fresh) type variables.
     -- Then insert all of these into the type env.
     let tenv' = foldl (\te (Var v, tv) -> insertTEnv te v (toScheme tv)) tenv (L.zip vs tvs)

     (s', bodyT) <- check tenv' s body

     -- May have discovered some arguments' type when body was checked, so apply
     -- new substitution. If not, use the fresh we got.
     let tvs' = map (apply s') tvs

     return (composeAll [s', s], TFun (tvs' ++ [bodyT]))

  NullaryApp (Var v) -> do
    (s1, t) <- check tenv s (Var v)
    tv <- getFresh
    s2 <- unify t (TFun [tv])
    return (composeAll [s2, s1, s], apply s2 tv)
  NullaryApp (Fun [] body) -> check tenv s body
  NullaryApp body -> check tenv s body >>= (throwE . FunctionExpected . snd)

  -- To check the type, build two Fun types, one for @fn@ and one for
  -- @body@. Then attempt to unify.
  UnaryApp fn body -> do
    (s1, fnT) <- check tenv s fn
    (s2, bodyT) <- check tenv s1 body
    retT <- getFresh

    s3 <- unify (apply s2 fnT) (apply s2 (TFun (bodyT : [retT])))
    return (composeAll [s3, s2, s1, s], apply s3 retT)

  -- TODO:
  --
  -- - get fresh type vars for tvs and the ones in ctors, so that they don't
  --   collide w/ the ones in the current env.
  --
  -- - verify the type vars used in ctors are in `tvs`
  --
  -- - This returns type Unit, but its constructors needs to be added to the
  -- TEnv. But how do we do this? We CAN use another env, like a data
  -- constructor env.
  --
  -- Or, assume that data type defs are handled BEFORE type checking. They are a
  -- top-level construct, so should assume to be correct here.
  DefDataType n tvs ctors -> return (s, TUnit)

  -- TODO
  --
  -- - return a TDataType type
  DataCtor n ts -> return (s, TUnit)

  CtorApp n exprs -> return (s, TInt)

  BinOp{} -> error "Shouldn't need to type check BinOps."
  Def{} -> error "TODO"

getNumArgs :: Type -> Int
getNumArgs (TFun vs) = length vs - 1
getNumArgs _ = 0

isTFun :: Type -> Bool
isTFun TFun{} = True
isTFun _ = False

isListOfVars :: [Exp] -> Bool
isListOfVars [] = True
isListOfVars (Var _:vs) = isListOfVars vs
isListOfVars _ = False

-- | Rename and reorder type variables to look nice.
--
-- >>> runWithFreshCounter $ reorderTVars (TVar "b")
-- Right a
--
-- >>> runWithFreshCounter $ reorderTVars (TFun [TVar "c",TVar "b"])
-- Right (-> a b)
--
-- >>> runWithFreshCounter $ reorderTVars (TFun [TVar "c",TFun [TVar "c",TVar "b"],TVar "a"])
-- Right (-> a (-> a b) c)
--
-- >>> runWithFreshCounter $ reorderTVars (TFun [TVar "c",TList (TVar "c"),TVar "a"])
-- Right (-> a [a] b)
--
-- >>> runWithFreshCounter $ reorderTVars (TFun [TFun [TVar "b",TVar "c"],TFun [TVar "b",TVar "c"]])
-- Right (-> (-> a b) (-> a b))
--
reorderTVars :: Type -> TypeCheck Type
reorderTVars t = liftM snd (freshenWithSubst emptySubst (generalize emptyTEnv emptySubst t))

-- | Insert fresh variables for universally-quantified types.
--
-- >>> runWithFreshCounter (freshen (Forall ["x","y"] (TFun [TVar "x",TVar "y",TVar "c"])))
-- Right (-> a b c)
--
freshen :: TypeScheme -> TypeCheck Type
freshen ts = liftM snd (freshenWithSubst emptySubst ts)

-- | Insert fresh variables for universally-quantified types, given a Subst
-- context.
--
-- >>> runWithFreshCounter (freshenWithSubst (M.fromList [("x",TInt)]) (Forall ["x","y"] (TFun [TVar "x",TVar "y",TVar "c"])))
-- Right (fromList [("x",Int),("y",a)],(-> Int a c))
--
freshenWithSubst :: Subst -> TypeScheme -> TypeCheck (Subst, Type)
freshenWithSubst s (Forall utvs (TVar tv)) =
  if tv `elem` utvs
  then case M.lookup tv s of
       Just ftv -> return (s, ftv)
       Nothing -> do
         ftv <- getFresh
         return (M.insert tv ftv s, ftv)
  else return (s, TVar tv)

-- Perhaps this is the simpler solution, compared to below.
-- freshenWithSubst s (Forall _ (TFun [])) = return (s, TFun [])
-- freshenWithSubst s (Forall utvs (TFun [v])) = do
--   (s1, v1) <- freshenWithSubst s (Forall utvs v)
--   return (s1, TFun [v1])
-- freshenWithSubst s (Forall utvs (TFun (v:vs))) = do
--   (s1, v1) <- freshenWithSubst s (Forall utvs v)
--   (s2, v2) <- freshenWithSubst s1 (Forall utvs (TFun vs))
--   return (s2, TFun (v1:(getTFunArgs v2)))

freshenWithSubst s (Forall utvs (TFun vs)) = do
  -- For each argument variable:
  --
  --   * Grab the previous substitution and the already-freshened type vars list
  --   * Freshen the current type
  --   * Append that type to the already-freshened type vars list
  --
  -- For example, if we have @Fun [x y z]@, we will convert @x@ to @a@,
  -- then @y@ to @b@, and so on. This will be built in the list as @[c b a]@.
  -- Note it is backwards, since we use a foldl (and not foldr since we want
  -- fresh variables in order). This is why we reverse it at the end.
  --
  (s', vs') <-
    foldl
      (\tc v -> do
        (s1, sumArgs) <- tc
        (s2, v1) <- freshenWithSubst s1 (Forall utvs v)
        return (s2, v1:sumArgs))
      (return (s, []))
      vs
  return (s', TFun (reverse vs'))

freshenWithSubst s (Forall utvs (TList tv)) = do
  (s1, tv1) <- freshenWithSubst s (Forall utvs tv)
  return (s1, TList tv1)
freshenWithSubst s (Forall _ t) = return (s, t)

------------------------------------------
-- Utilities
------------------------------------------

getTFunArgs :: Type -> [Type]
getTFunArgs (TFun args) = args
getTFunArgs _ = error "not TFun"

defaultTEnv :: M.Map Name TypeScheme
defaultTEnv = M.fromList [
  ("+", Forall [] (TFun [TInt,TInt,TInt]))
 ,("-", Forall [] (TFun [TInt,TInt,TInt]))
 ,("*", Forall [] (TFun [TInt,TInt,TInt]))

 ,("and", Forall [] (TFun [TBool,TBool,TBool]))
 ,("or", Forall [] (TFun [TBool,TBool,TBool]))
 ,("xor", Forall [] (TFun [TBool,TBool,TBool]))
  ]

emptyTEnv :: TEnv
emptyTEnv = M.empty

emptySubst :: Subst
emptySubst = M.empty

lookupTEnv :: TEnv -> Name -> Maybe TypeScheme
lookupTEnv tenv name = M.lookup name tenv

insertTEnv :: TEnv -> Name -> TypeScheme -> TEnv
insertTEnv tenv name t = M.insert name t tenv

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

-- | Get fresh variable.
--
-- >>> runWithFreshCounter getFresh
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

toScheme :: Type -> TypeScheme
toScheme = Forall []

toTName :: Type -> TName
toTName (TVar t) = t
toTName _ = error "Not TVar!"

emptyGenericTypeError :: TypeError
emptyGenericTypeError = GenericTypeError Nothing

genericTypeError :: String -> TypeError
genericTypeError msg = GenericTypeError (Just msg)

initFreshCounter :: FreshCounter
initFreshCounter = FreshCounter { getFreshCounter = 0 }

------------------------------------------
-- Helpers to run the monad transformers.
------------------------------------------

runUnify :: TypeCheck Subst -> Either TypeError Subst
runUnify uc = evalState (runExceptT uc) initFreshCounter

runCheck :: TEnv -> Subst -> Exp -> (Subst, Type)
runCheck tenv s expr =
  case evalState (runExceptT (check tenv s expr)) initFreshCounter of
    Left e -> error (show e)
    Right r -> r

runWithFreshCounter :: ExceptT e (State FreshCounter) a -> Either e a
runWithFreshCounter e = evalState (runExceptT e) initFreshCounter

------------------------------------------
-- Show instances
------------------------------------------

instance Show TypeScheme where
  show (Forall tvs t) = "∀:" ++ show tvs ++ " " ++ show t

instance Show TypeError where
  show (Mismatch t1 t2) = "type mismatch: expecting " ++ show t1 ++ " but got " ++ show t2
  show (FunctionExpected t) = "type mismatch: expecting function but got " ++ show t
  show (UnboundVariable n) = "unbound variable " ++ n
  show (InfiniteType tvar t) = "cannot resolve infinite type " ++ show tvar ++ " in " ++ show t
  show (GenericTypeError (Just msg)) = "type error: " ++ msg
  show (GenericTypeError Nothing) = "type error"

