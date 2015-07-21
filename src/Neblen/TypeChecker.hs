{-# LANGUAGE OverloadedStrings #-}

module Neblen.TypeChecker where

import Neblen.Data
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.State

-- Mapping of variables to its type.
type TEnv = M.Map Name Type

-- Type variable.
type TName = String

-- Unification tenvironment. Mapping of type variables to its type.
type UEnv = M.Map TName Type

-- TODO extract literal types to TLit, similar to "Literal" for expressions?
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
               | UnboundVariable Name
               | InfiniteType Type Type -- InfiteType TVar Type
               | GenericTypeError

  deriving (Show)

newtype FreshCounter = FreshCounter { getFreshCounter :: Int }

initFreshCounter :: FreshCounter
initFreshCounter = FreshCounter { getFreshCounter = 0 }

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

-- | Get fresh variable.
--
-- >>> evalState getFresh initFreshCounter
-- a
--
-- >>> evalState getFresh (FreshCounter { getFreshCounter = 25 })
-- z
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
-- >>> unify emptyUEnv (TVar "a") TInt
-- (fromList [("a",TInt)],TInt)
--
-- >>> unify emptyUEnv TInt (TVar "a")
-- (fromList [("a",TInt)],TInt)
--
-- >>> unify (M.fromList [("a",TInt)]) (TVar "a") (TVar "a")
-- (fromList [("a",TInt)],TInt)
--
-- >>> unify (M.fromList [("a",TInt)]) (TVar "a") (TVar "b")
-- (fromList [("a",TInt),("b",TInt)],TInt)
--
-- >>> unify emptyUEnv (TFun TInt TInt) (TFun (TVar "a") (TVar "b"))
-- (fromList [("a",TInt),("b",TInt)],TFun TInt TInt)
--
-- Some complex ones:
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
-- >>> unify (M.fromList [("a",TBool)]) TInt (TVar "a")
-- *** Exception: type mismatch: expecting TBool but got TInt
--
-- >>> unify emptyUEnv (TFun (TVar "a") (TVar "a")) (TFun (TVar "a") (TVar "a"))
-- (fromList [],TFun (TVar "a") (TVar "a"))
--
-- >>> unify (M.fromList [("a",TInt)]) (TFun (TVar "a") (TVar "b")) (TFun (TVar "a") (TVar "b"))
-- (fromList [("a",TInt)],TFun TInt (TVar "b"))
--
-- Failure cases:
--
-- >>> unify (M.fromList [("a",TBool)]) (TVar "a") TInt
-- *** Exception: type mismatch: expecting TBool but got TInt
--
-- >>> unify (M.fromList [("a",TBool)]) TInt (TVar "a")
-- *** Exception: type mismatch: expecting TBool but got TInt
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
  in (uenv'', TFun (replaceTVarInEnv uenv'' ta) (replaceTVarInEnv uenv'' tr))
unify _ t1 t2 = error $ "type mismatch: expecting " ++ show t1 ++ " but got " ++ show t2

unifyTVar :: UEnv -> Type -> Type -> (UEnv, Type)
unifyTVar uenv (TVar tv) t2 =
  case lookupEnv uenv tv of
    Nothing ->
      case t2 of
        -- If both sides are free, then see if we can resolve the RHS with the
        -- u-env. For example, in (-> a a) (-> a b) with uenv {b -> Int}, the
        -- LHS failed to resolve, so we try the RHS.
        TVar tv' -> case lookupEnv uenv tv' of
                      -- Can't resolve RHS, so just return free.
                      Nothing -> (uenv, TVar tv')

                      -- Resolved RHS. In the example above, b (RHS) is resolved to
                      -- Int. So we need to update uenv to {b -> Int, a -> Int}
                      Just t' -> let uenv' = (insertEnv uenv tv' t')
                                     uenv'' = (insertEnv uenv' tv t')
                                 in unify (insertEnv uenv'' tv' t') (TVar tv') t'
        _      -> let uenv' = insertEnv uenv tv t2
                  in unify uenv' (TVar tv) t2
    Just t ->
      unify uenv t t2
unifyTVar _ _ _ = error "bad call to unifyTVar"

  -- harvest all the free variables.
  -- permute through all possible configurations.
  --
--
-- TODO: This needs a unifier to realize that a = stillfree = TInt.
--
-- >>> checkExp (M.fromList [("x", TFun (TVar "a") (TVar "a"))]) (UnaryCall (Function (Var "y") (UnaryCall (Var "y") (Literal (IntV 3)))) (Var "x"))
-- *** Exception: type mismatch: expecting TFun TInt (TVar "stillfree") but got TFun (TVar "a") (TVar "a")
-- ^ This should unify to: TFun TInt TInt

-- Below is: ((fn [x] x 3) (fn [x] x))
--                         (-> a a)
--            (fn [x : (-> a a)] x 3)
--
-- TODO: This needs a unifier:
--
-- >>> checkExp emptyTEnv (UnaryCall (Function (Var "x") (UnaryCall (Var "x") (Literal (IntV 3)))) (Function (Var "x") (Var "x")))
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

replaceTVarInEnv :: UEnv -> Type -> Type
replaceTVarInEnv uenv (TVar name) = fromMaybe (TVar name) (lookupEnv uenv name)
replaceTVarInEnv _ t = t

insertEnv :: TEnv -> Name -> Type -> TEnv
insertEnv tenv name t = M.insert name t tenv

-- >>> checkExp emptyTEnv (Literal (IntV 0))
-- (fromList [],TInt)
--
-- >>> checkExp emptyTEnv (Literal (BoolV 0))
-- (fromList [],TBool)
--
-- >>> checkExp emptyTEnv (Literal (StringV 0))
-- (fromList [],TString)
--
checkLiteral :: TEnv -> UEnv -> Exp -> (TEnv, UEnv, Type)
checkLiteral tenv uenv (Literal (IntV _)) = (tenv, uenv, TInt)
checkLiteral tenv uenv (Literal (BoolV _)) = (tenv, uenv, TBool)
checkLiteral tenv uenv (Literal (StringV _)) = (tenv, uenv, TString)
checkLiteral _ _ _ = error "Invalid"

-- | Check variable.
--
-- >>> checkExp (M.fromList [("x",TInt)]) (Var "x")
-- (fromList [("x",TInt)],TInt)
--
-- >>> checkExp emptyTEnv (Var "x")
-- *** Exception: unbounded variable
--
checkVar :: TEnv -> UEnv -> Exp -> (TEnv, UEnv, Type)
checkVar tenv uenv (Var v) =
  case lookupEnv tenv v of
    Just t  -> (tenv, uenv, t)
    Nothing -> error "unbounded variable"
checkVar _ _ _ = error "wrong type"

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
checkLet :: TEnv -> UEnv -> Exp -> (TEnv, UEnv, Type)
checkLet tenv uenv (Let (Var v) val body) =
  let (tenv', uenv', valT) = checkExp tenv uenv val
      tenv'' = insertEnv tenv' v valT
      (_, uenv'', valB) = checkExp tenv'' uenv' body
  -- Return original tenv because 'v' is no longer in scope.
  in (tenv, uenv, valB)
checkLet _ _ _ = error "wrong type"

-- | Check nullary function.
--
-- >>> checkNullFun emptyTEnv (NullaryFun (Literal (IntV 0)))
-- (fromList [],TInt)
--
-- >>> checkNullFun emptyTEnv (NullaryFun (Var "x"))
-- *** Exception: unbounded variable
--
checkNullFun :: TEnv -> UEnv -> Exp -> (TEnv, UEnv, Type)
checkNullFun tenv uenv (NullaryFun body) = checkExp tenv uenv body
checkNullFun _ _ _ = error "wrong type"

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
-- Below is: (fn [x] x x)
-- >>> checkExp emptyTEnv (Function (Var "x") (UnaryCall (Var "x") (Var "x")))
-- (fromList [],TFun (TVar "x") (TFun (TVar "x") (TVar "x")))
--
-- Below is: (fn [x] x 3)
-- >>> checkExp emptyTEnv (Function (Var "x") (UnaryCall (Var "x") (Literal (IntV 3))))
-- (fromList [],TFun (TFun TInt (TVar "stillfree")) (TVar "stillfree"))
--
checkFun :: TEnv -> UEnv -> Exp -> (TEnv, UEnv, Type)
checkFun tenv uenv (Function (Var v) body) =
  let tenv' = insertEnv tenv v (TVar v)
  -- ^ Set argument as free
      (tenv'', uenv', bodyT) = checkExp tenv' uenv body
  -- ^ Check body
      argT           = fromMaybe (TVar v) (lookupEnv tenv'' v)
  -- ^ May have out argument's type when body was checked.
  in (tenv, uenv, TFun argT bodyT)
checkFun _ _ _ = error "wrong type"

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
checkNullCall :: TEnv -> UEnv -> Exp -> (TEnv, UEnv, Type)
checkNullCall tenv uenv (NullaryCall (Var v)) = checkExp tenv uenv (Var v)
checkNullCall tenv uenv (NullaryCall (NullaryFun body)) = checkExp tenv uenv (NullaryFun body)
checkNullCall _ _ _ = error "wrong type"

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
-- Below is: ((fn [x] x 3) (fn [x] x))
-- checkExp emptyTEnv (UnaryCall (Function (Var "x") (UnaryCall (Var "x") (Literal (IntV 3)))) (Function (Var "x") (Var "x")))
-- TODO
--
-- Below is self application (see Pierce pg 345):
--   Environment: f : (-> a a)
--   ((fn [x] x x) f) : (-> (-> a a) (-> a a))
--
-- >>> checkExp (M.fromList [("f",TFun (TVar "a") (TVar "a"))]) (UnaryCall (Function (Var "x") (UnaryCall (Var "x") (Var "x"))) (Var "f"))
-- (fromList [("f",TFun (TVar "a") (TVar "a"))],TFun (TFun (TVar "a") (TVar "a")) (TFun (TVar "a") (TVar "a")))
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

-- Below is:
--   Environment:
--     x : (-> a a)
--  ((fn [y] y 3) x) : Int
--
-- TODO:
--   This needs a unifier to realize that a = stillfree = TInt.
--
-- >>> checkExp (M.fromList [("x", TFun (TVar "a") (TVar "a"))]) emptyTEnv (UnaryCall (Function (Var "y") (UnaryCall (Var "y") (Literal (IntV 3)))) (Var "x"))
-- *** Exception: type mismatch: expecting TFun TInt (TVar "stillfree") but got TFun (TVar "a") (TVar "a")

-- Below is: ((fn [x] x 3) (fn [x] x))
--                         (-> a a)
--            (fn [x : (-> a a)] x 3) : Int
--
-- TODO: This needs a unifier:
--
-- >>> checkExp emptyTEnv (UnaryCall (Function (Var "x") (UnaryCall (Var "x") (Literal (IntV 3)))) (Function (Var "x") (Var "x")))
-- *** Exception: type mismatch: expecting TFun TInt (TVar "stillfree") but got TFun (TVar "x") (TVar "x")
--
checkUnaryCall :: TEnv -> UEnv -> Exp -> (TEnv, UEnv, Type)
checkUnaryCall tenv uenv (UnaryCall fn arg) =
  let (_, uenv', fnT) = checkExp tenv uenv fn
  in case fnT of
       (TFun a b) ->
         let (_, uenv'', argT) = checkExp tenv uenv' arg
             (uenv''', ta) = unify uenv' a argT
         in (tenv, uenv''', b)
         -- in if isBound a && a /= argT
         --    -- Function doesn't take the type given by the argument body:
         --    -- ((-> Bool Int) Int) => error
         --    then error $ "type mismatch: expecting " ++ show a ++ " but got " ++ show argT

         --    else if isFree a && a == b
         --    -- If a and b are the same free variable, return the argument type:
         --    -- ((-> a a) Int) => ((-> Int Int) Int) => Int
         --    then (tenv, argT)

         --    else if isFree a && isBound argT
         --    -- If a is free and arg is bound, bound a to that type:
         --    -- ((-> a (-> b a)) Int) => ((-> Int (-> b Int)) Int) => (-> b Int)
         --    then let b' = bindFree (typeVarName a) argT b in (tenv, b')

         --    -- Else, return b whether free or bound:
         --    --
         --    -- ((-> a b) Int) => ((-> Int b) Int) => b
         --    -- ((-> a Bool) Int) => ((-> Int Bool) Int) => Bool
         --    --
         --    else (tenv, b)

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
       (TVar vT) ->
         let (_, uenv'', argT) = checkExp tenv uenv' arg
         in if isBound argT
            then let retT = TVar "stillfree"
                     -- We partially know the type of the function. Add that to
                     -- the tenv so that parent can use it in their type check.
                     tenv' = insertEnv tenv vT (TFun argT retT)
                 in (tenv', uenv'', retT)
            else (tenv, uenv'', TFun (TVar "free") (TVar "free"))
       _ -> error "calling a non-function"
checkUnaryCall _ _ _ = error "wrong type"

-- | Check list.
--
-- >>> checkList emptyTEnv (List [Literal (IntV 0)])
-- (fromList [],TList TInt)
--
-- Below is: (list 0 ((fn [x] x) 0))
-- >>> checkList emptyTEnv (List [Literal (IntV 0),UnaryCall (Function (Var "x") (Var "x")) (Literal (IntV 0))])
-- (fromList [],TList TInt)
--
-- >>> checkList emptyTEnv (List [Literal (IntV 0),Literal (BoolV True)])
-- *** Exception: list type mismatch: TInt and TBool found
--
-- >>> checkList emptyTEnv (List [])
-- TODO: need to use a TVar with non-clashing variable name.
--
checkList :: TEnv -> UEnv -> Exp -> (TEnv, UEnv, Type)
checkList tenv uenv (List []) = (tenv, uenv, TList TFree)
checkList tenv uenv (List [e]) =
  let (_, uenv', eT) = checkExp tenv uenv e
  in (tenv, uenv', TList eT)
checkList tenv uenv (List (e1:e2:es)) =
  let (_, uenv', e1T) = checkExp tenv uenv e1
      (_, uenv'', e2T) = checkExp tenv uenv' e2
  in if e1T == e2T
       then checkList tenv uenv (List (e2:es))
       else error $ "list type mismatch: " ++ show e1T ++ " and " ++ show e2T ++ " found"
checkList _ _ _ = error "wrong type"

-- | Check vector.
--
-- TODO use vectors but still share code with checkList.
checkVector :: TEnv -> UEnv -> Exp -> (TEnv, UEnv, Type)
checkVector = checkList

-- | Check if.
--
-- Below is: (if ((fn [x] true) 0) "truth" "false") : String
-- >>> checkIf emptyTEnv (If (UnaryCall (Function (Var "x") (Literal (BoolV True))) (Literal (IntV 0))) (Literal (StringV "then clause")) (Literal (StringV "else clause")))
-- (fromList [],TString)
--
-- Below is: (if false 0 false)
-- >>> checkIf emptyTEnv (If (Literal (BoolV False)) (Literal (IntV 0)) (Literal (BoolV False)))
-- *** Exception: then and else clause type mismatch: TInt and TBool found
--
checkIf :: TEnv -> UEnv -> Exp -> (TEnv, UEnv, Type)
checkIf tenv uenv (If p t e) =
  let (_, uenv', pT) = checkExp tenv uenv p
  in case pT of
       TBool ->
         let (_, uenv'', tT) = checkExp tenv uenv' t
             (_, uenv''', eT) = checkExp tenv uenv'' e
         in if tT == eT
            then (tenv, uenv''', eT)
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
-- >>> checkExp emptyTEnv (If (Literal (BoolV False)) (Literal (IntV 0)) (Literal (IntV 0)))
-- (fromList [],TInt)
--
checkExp :: TEnv -> UEnv -> Exp -> (TEnv, UEnv, Type)
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

