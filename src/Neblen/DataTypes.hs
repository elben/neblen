{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Neblen.DataTypes where

import Neblen.Data
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.State
import Control.Monad (liftM)

-- = Data types and kind evaluator
--
-- TODO explain how kind-finding works!
--
-- Neblen supports data types similar to Haskell.
--

-- | Data type declarations. Separated out from regular expressions.
--
-- data Declare = DeclareType Name [TName] [Declare] Kind
--              | DeclareCtor Name [Type]
--   deriving (Show, Eq, Ord)

-- | Mapping of type variable (from TVarK) to kind.
type KEnv = M.Map TName Kind

-- | Mapping of unknown kind variables to kinds.
type KSubst = M.Map Int Kind

-- | Mapping of data type constant to kind. (e.g. Either : * -> * -> *)
type KConstEnv = M.Map TName Kind

type KindCheck a = State FreshCounter a

-- | Evaluate a type declaration. Returns a new data type environment, and the
-- current data type with its kinds filled out.
--
-- = Examples:
--
-- Person, a simple data type:
--
--   data Person = Person String Int
--   (data-type Person (Person String Int))
--
-- >>> evalState (evalDataTypeKind M.empty M.empty (DeclareType "Person" [] [DeclareCtor "Person" [TString,TInt]] (KUnknown 0))) (initFreshCounterAt 10)
-- (fromList [],DeclareType "Person" [] [DeclareCtor "Person" [String,Int]] *)
--
-- Maybe:
--
--  data Maybe a = Nothing | Just a
--  (data-type Maybe (a) Nothing (Just a))
--
-- >>> evalState (evalDataTypeKind M.empty M.empty (DeclareType "Maybe" ["a"] [DeclareCtor "Nothing" [], DeclareCtor "Just" [TVarK "a" (KUnknown 0)]] (KUnknown 1))) (initFreshCounterAt 10)
-- (fromList [("a",*)],DeclareType "Maybe" ["a"] [DeclareCtor "Nothing" [],DeclareCtor "Just" [a : k0]] (* -> *))
--
-- Either:
--
--   data Either a b = Left a | Right b
--   (data-type Either (a b) (Left a) (Right b))
--
-- >>> evalState (evalDataTypeKind M.empty M.empty (DeclareType "Either" ["a", "b"] [DeclareCtor "Left" [TVarK "a" (KUnknown 0)], DeclareCtor "Just" [TVarK "b" (KUnknown 1)]] (KUnknown 2))) (initFreshCounterAt 10)
-- (fromList [("a",*),("b",*)],DeclareType "Either" ["a","b"] [DeclareCtor "Left" [a : k0],DeclareCtor "Just" [b : k1]] (* -> (* -> *)))
--
-- ExceptT:
--
-- >>> :{
-- evalState
--   (evalDataTypeKind (M.fromList [("Either",KFun Star (KFun Star Star))]) M.empty
--     (DeclareType "ExceptT" ["e","m","a"] [
--       DeclareCtor "ExceptT" [
--         TApp (TVarK "m" (KUnknown 0))
--              (TApp (TApp (TConst "Either" (KUnknown 1)) (TVarK "e" (KUnknown 2)))
--              (TVarK "a" (KUnknown 3)))]]
--       (KUnknown 4)))
--   (initFreshCounterAt 10)
-- :}
-- (fromList [("a",*),("e",*),("m",(* -> *))],DeclareType "ExceptT" ["e","m","a"] [DeclareCtor "ExceptT" [(m : k0 ((Either e : k2) a : k3))]] (* -> ((* -> *) -> (* -> *))))
--
-- Tree:
--
--   data Tree a = Leaf a | Branch (Tree a) (Tree a)
--   (data-type Tree (a) (Leaf a) (Branch (Tree a) (Tree a)))
--
-- >>> :{
-- evalState
--   (evalDataTypeKind M.empty M.empty
--     (DeclareType "Tree" ["a"] [
--       DeclareCtor "Leaf" [TVarK "a" (KUnknown 0)],
--       DeclareCtor "Branch" [
--         TApp (TConst "Tree" (KUnknown 1)) (TVarK "a" (KUnknown 0)),
--         TApp (TConst "Tree" (KUnknown 1)) (TVarK "a" (KUnknown 0))]]
--      (KUnknown 1)))
--   (initFreshCounterAt 10)
-- :}
-- (fromList [("a",*)],DeclareType "Tree" ["a"] [DeclareCtor "Leaf" [a : k0],DeclareCtor "Branch" [(Tree a : k0),(Tree a : k0)]] (* -> *))
--
-- Complex type:
--
--   data Dallas a b c d e =  Dallas (a (b c) d e)
--
-- >>> :{
-- evalState
--   (evalDataTypeKind M.empty M.empty
--     (DeclareType "Complex" ["a","b","c","d"]
--       [DeclareCtor "Complex"
--         [TApp (TApp (TApp (TVarK "a" (KUnknown 0))
--                           (TApp (TVarK "b" (KUnknown 1)) (TVarK "c" (KUnknown 2))))
--                     (TVarK "d" (KUnknown 3)))
--               (TVarK "e" (KUnknown 4))]]
--       (KUnknown 5)))
--   (initFreshCounterAt 10)
-- :}
-- (fromList [("a",(* -> (* -> (* -> *)))),("b",(* -> *)),("c",*),("d",*),("e",*)],DeclareType "Complex" ["a","b","c","d"] [DeclareCtor "Complex" [(((a : k0 (b : k1 c : k2)) d : k3) e : k4)]] ((* -> (* -> (* -> *))) -> ((* -> *) -> (* -> (* -> *)))))
--
evalDataTypeKind ::
  KConstEnv
  -- ^ Mapping of data types to its kind
  -> KEnv
  -- ^ Do we need this? Mapping of type var to kind
  -> DeclareType
  -- ^ The data type we're trying to find the kind of
  -> KindCheck (KEnv, DeclareType)
  -- ^ Returns the new KEnv and DeclareType, with its kinds filled in
evalDataTypeKind cenv kenv (DeclareType name tvs ctors k) = do
  -- Add current data type (whose kind is unknown) into const env to handle
  -- recursive data types.
  let cenv2 = M.insert name k cenv
  -- Evaluate each constructor's kind, building up the substitution map.
  (kenv2, ksub2) <- foldl (\kindCheck ctor -> do
                               (kenv', ksub') <- kindCheck
                               (kenv'', ksub'') <- evalCtorKind cenv2 kenv' ksub' ctor
                               -- Compose the new substitution with the old one
                               return (kenv'', ksub'' `composeKSubst` ksub'))
                         (return (kenv, M.empty)) ctors
  -- Substitute all type variables with the final substitution mapping. Then,
  -- replace all unknown kinds with Star, since at this point there are no more
  -- substitutions to be made.
  let kenv3 = replaceWithStars (kapply ksub2 kenv2)
  -- Find the final kind of the data type, given the data type's type variables,
  -- and the finalized kind environment.
  let dataTypeKind = findFinalKind kenv3 tvs
  return (kenv3, DeclareType name tvs ctors dataTypeKind)

-- | Given a kind mapping, convert a list of type variables to a kind function
-- that takes each type variable in order.
--
-- >>> findFinalKind (M.fromList [("a",Star),("b",KFun Star Star)]) ["a","b"]
-- (* -> ((* -> *) -> *))
--
findFinalKind :: KEnv -> [TName] -> Kind
findFinalKind _ [] = Star
findFinalKind kenv (tv:tvs) =
  case M.lookup tv kenv of
    Just k -> KFun k (findFinalKind kenv tvs)
    Nothing -> error $ "could not find " ++ show tv

-- | Replace all unknown kinds with Star.
replaceWithStars :: M.Map k Kind -> M.Map k Kind
replaceWithStars = M.map replaceKindWithStars

-- | Replace all unknown kinds with Star.
replaceKindWithStars :: Kind -> Kind
replaceKindWithStars (KFun k1 k2) = KFun (replaceKindWithStars k1) (replaceKindWithStars k2)
replaceKindWithStars _ = Star

-- | Evaluate the kind of a data constructor.
--
-- >>> evalState (evalCtorKind M.empty M.empty M.empty (DeclareCtor "Just" [TVarK "a" (KUnknown 0)])) (initFreshCounterAt 1)
-- (fromList [("a",k0)],fromList [])
--
-- >>> evalState (evalCtorKind M.empty M.empty M.empty (DeclareCtor "Foo" [TApp (TVarK "a" (KUnknown 0)) (TVarK "b" (KUnknown 1)), TVarK "a" (KUnknown 0)])) (initFreshCounterAt 10)
-- (fromList [("a",(k1 -> k10)),("b",k1)],fromList [(0,(k1 -> k10))])
--
evalCtorKind :: KConstEnv -> KEnv -> KSubst -> DeclareCtor -> KindCheck (KEnv, KSubst)
evalCtorKind cenv kenv ksub (DeclareCtor _ types) = do
  (kenv2, ksub2) <- foldl (\kindCheck t -> do
                             (kenv', ksub') <- kindCheck
                             (kenv'', ksub'', _) <- evalKindOfType cenv kenv' ksub' t
                             return (kenv'', ksub'' `composeKSubst` ksub'))
                      (return (kenv, ksub)) types
  return (kapply ksub2 kenv2, ksub2)

-- | Evaluate kind of type.
--
-- >>> evalState (evalKindOfType M.empty M.empty M.empty (TVarK "a" (KUnknown 0))) (initFreshCounterAt 10)
-- (fromList [("a",k0)],fromList [],k0)
--
-- >>> evalState (evalKindOfType (M.fromList [("Foo", KUnknown 0)]) M.empty M.empty (TConst "Foo" (KUnknown 0))) (initFreshCounterAt 10)
-- (fromList [],fromList [],k0)
--
-- For below, we have:
--
--   KEnv:
--   a : 0
--   b : 1
--
--   KSubst:
--   empty
--
-- Expected result:
--
--   KEnv:
--   a : 1 -> 10
--   b : 1
--
--   KSubst:
--   0 : 1 -> 10
--
-- >>> evalState (evalKindOfType M.empty (M.fromList [("a",KUnknown 0), ("b",KUnknown 1)]) M.empty (TApp (TVarK "a" (KUnknown 0)) (TVarK "b" (KUnknown 1)))) (initFreshCounterAt 10)
-- (fromList [("a",(k1 -> k10)),("b",k1)],fromList [(0,(k1 -> k10))],k10)
--
-- For below, we have:
--
--   KEnv:
--   a : 0
--   b : 1
--   c : 2
--   d : 3
--
--   KSubst:
--   empty
--
-- Expected results:
--
--   KEnv:
--   a : 10 -> (3 -> 12)
--   b : 2 -> 10
--   c : 2
--   d : 3
--
--   KSubst:
--   0 : 10 -> (3 -> 12)
--   1 : 2 -> 10
--   11: 3 -> 12
--
-- >>> evalState (evalKindOfType M.empty (M.fromList [("a",KUnknown 0), ("b",KUnknown 1), ("c",KUnknown 2), ("d",KUnknown 3)]) M.empty (TApp (TApp (TVarK "a" (KUnknown 0)) (TApp (TVarK "b" (KUnknown 1)) (TVarK "c" (KUnknown 2)))) (TVarK "d" (KUnknown 3)))) (initFreshCounterAt 10)
-- (fromList [("a",(k10 -> (k3 -> k12))),("b",(k2 -> k10)),("c",k2),("d",k3)],fromList [(0,(k10 -> (k3 -> k12))),(1,(k2 -> k10)),(11,(k3 -> k12))],k12)
--
evalKindOfType :: KConstEnv -> KEnv -> KSubst -> Type -> KindCheck (KEnv, KSubst, Kind)
evalKindOfType _    kenv ksub (TVarK tv (KUnknown kv)) = return (M.insert tv (KUnknown kv) kenv, ksub, KUnknown kv)
evalKindOfType cenv kenv ksub (TConst name k) =
  if M.member name cenv
  then return (kenv, ksub, k)
  else error ("Unknown TConst: " ++ show (TConst name k))
evalKindOfType cenv kenv ksub (TApp t1 t2) = do
  -- Find kind and environment of LHS and RHS
  (kenv1, ksub1, k1) <- evalKindOfType cenv kenv ksub t1
  (kenv2, ksub2, k2) <- evalKindOfType cenv kenv1 (ksub1 `composeKSubst` ksub) t2
  -- Get a fresh kind variable; lifts `KUnknown` function into the State monad
  -- with the fresh integer.
  kv <- liftM KUnknown nextFreshCounter
  -- Compose all kind variable (integer) substitutions together
  let ksub3 = M.singleton (getKindVar k1) (KFun k2 kv) `composeKSubst` ksub2 `composeKSubst` ksub1
  return (kapply ksub3 kenv2, ksub3 `composeKSubst` ksub3, kv)
evalKindOfType _ kenv ksub TInt = return (kenv, ksub, Star)
evalKindOfType _ kenv ksub TBool = return (kenv, ksub, Star)
evalKindOfType _ kenv ksub TString = return (kenv, ksub, Star)
evalKindOfType _ _ _ t = error $ "Unsupported type: " ++ show t

getKindVar :: Kind -> Int
getKindVar (KUnknown i) = i
getKindVar _ = error "Should not be called on known kind."

nextFreshCounter :: KindCheck Int
nextFreshCounter = do
  s <- get -- Same as: ExceptT (liftM Right get)
  put s{getFreshCounter = getFreshCounter s + 1}
  return $ getFreshCounter s

-- Given a data type declaration, return a monad in which the KUnknowns are replaced.
replaceKUnknownInitsDeclareType :: DeclareType -> KindCheck DeclareType
replaceKUnknownInitsDeclareType (DeclareType name tvs ctors k) = do
  ctors' <- mapM replaceKUnknownsInDeclareCtorWith ctors
  k' <- replaceKUnknownInit k
  return (DeclareType name tvs ctors' k')

replaceKUnknownsInDeclareCtorWith :: DeclareCtor -> KindCheck DeclareCtor
replaceKUnknownsInDeclareCtorWith (DeclareCtor name types) = do
  filledTypes <- mapM replaceKUnknownTVarK types
  return (DeclareCtor name filledTypes)

replaceKUnknownTVarK :: Type -> KindCheck Type
replaceKUnknownTVarK (TVarK name k) = do
  k' <- replaceKUnknownInit k
  return $ TVarK name k'
replaceKUnknownTVarK tvark = return tvark

replaceKUnknownInit :: Kind -> KindCheck Kind
replaceKUnknownInit KUnknownInit = liftM KUnknown nextFreshCounter
replaceKUnknownInit k = return k

-- TODO can eventually combine as `Substitutable a t`?
class KSubstitutable k where
  kapply :: KSubst -> k -> k

instance KSubstitutable Kind where
  kapply _ Star = Star
  kapply ksub (KFun k1 k2) = KFun (kapply ksub k1) (kapply ksub k2)
  kapply ksub (KUnknown i) = fromMaybe (KUnknown i) (M.lookup i ksub)
  kapply _ KUnknownInit = KUnknownInit

instance KSubstitutable KEnv where
  kapply ksub = M.map (kapply ksub)

instance KSubstitutable DeclareType where
  -- Replace unknown kinds with the given substitutions
  kapply ksub (DeclareType name tvs ctors kind) =
    DeclareType
      name tvs
      (map (kapply ksub) ctors)
      (kapply ksub kind)

instance KSubstitutable DeclareCtor where
  kapply ksub (DeclareCtor name types) = DeclareCtor name (map (kapply ksub) types)

instance KSubstitutable Type where
  kapply ksub (TConst name kind) = TConst name (kapply ksub kind)
  kapply ksub (TVarK name kind) = TVarK name (kapply ksub kind)
  kapply ksub (TApp t1 t2) = TApp (kapply ksub t1) (kapply ksub t2)
  kapply _ t = t

-- | Compose KSubst, applying @ksub1@'s substitutions over values of @ksub2@.
--
composeKSubst :: KSubst -> KSubst -> KSubst
composeKSubst ksub1 ksub2 = M.union (M.map (kapply ksub1) ksub2) ksub1
