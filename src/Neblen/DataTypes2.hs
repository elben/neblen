module Neblen.DataTypes2 where

import Neblen.Data
import Neblen.DataTypes
import qualified Data.Map.Strict as M

import Debug.Trace

-- Clean slate to think about TApp

-- data ExceptT e m a = m (Either e a)
--
dtExceptT' :: Declare
dtExceptT' = DeclareType "ExceptT" ["e","m","a"]
                        [DeclareCtor "ExceptT"
                          [TApp (TVarK "m" (KUnknown 0))
                                (TApp (TApp (TConst "Either" (KUnknown 1)) (TVarK "e" (KUnknown 2)))
                                      (TVarK "a" (KUnknown 3)))]]
            (KUnknown 4)

-- data State s a = State (-> s (Pair s a))
--
dtState' :: Declare
dtState' = DeclareType "State" ["s","a"]
                      [DeclareCtor "State"
                        [TApp (TApp (TConst "->" (KUnknown 0))
                                    (TVarK "s" (KUnknown 1)))
                              (TApp (TApp (TConst "Pair" (KUnknown 2)) (TVarK "s" (KUnknown 1)))
                                    (TVarK "a" (KUnknown 2)))]]
                      (KUnknown 3)

-- Recursive data type:
data Tree a = Leaf a
            | Branch (Tree a) (Tree a)

dtTree :: Declare
dtTree = DeclareType "Tree" ["a"]
                     [DeclareCtor "Leaf" [TVarK "a" (KUnknown 0)],
                      DeclareCtor "Branch" [TApp (TConst "Tree" (KUnknown 1)) (TVarK "a" (KUnknown 0)),
                                            TApp (TConst "Tree" (KUnknown 1)) (TVarK "a" (KUnknown 0))]]
                     (KUnknown 1)

-- Foo :: (* -> * -> *) -> * -> * -> *
--
data Dallas a b c d e =  Dallas (a (b c) d e)
--
dtFoo :: Declare
dtFoo = DeclareType "Foo" ["a","b","c","d"]
          [DeclareCtor "Foo"
            [TApp (TApp (TApp (TVarK "a" (KUnknown 0))
                              (TApp (TVarK "b" (KUnknown 1)) (TVarK "c" (KUnknown 2))))
                        (TVarK "d" (KUnknown 3)))
                  (TVarK "e" (KUnknown 4))]]
          (KUnknown 5)

foo = Dallas

---------------------------
-- Let's manually do some evaluation.
---------------------------
--
---------------------------
-- Except
---------------------------
--
-- data ExceptT e m a = m (Either e a)
--
-- dtExceptT :: Declare
-- dtExceptT = DeclareType "ExceptT" ["e","m","a"]
--                         [DeclareCtor "ExceptT"
--                           [TApp (TVarK "m" (KUnknown 0))
--                                 (TApp (TApp (TConst "Either" (KUnknown 1)) (TVarK "e" (KUnknown 2)))
--                                       (TVarK "a" (KUnknown 3)))]]
--             (KUnknown 4)
--
-- Starts with inner-most lhs:
--
-- ===== (TVarK "m" (KUnknown 0))
--
-- KConstEnv:
-- Either : * -> * -> *
-- ExceptT : 4
--
-- KEnv:
-- m : 0
--
-- KSubst:
-- n/a
--
-- ===== (TConst "Either" (KUnknown 1))
--
-- KConstEnv:
-- Either : * -> * -> *
-- ExceptT : 4
--
-- KEnv:
-- m : 0
--
-- KSubst:
--
-- ===== (TVarK "e" (KUnknown 2))
--
-- KConstEnv:
-- Either : * -> * -> *
-- ExceptT : 4
--
-- KEnv:
-- m : 0
-- e : 2
--
-- KSubst:
--
-- ===== (TApp (TConst "Either" (KUnknown 1)) (TVarK "e" (KUnknown 2)))
--
-- We know to apply, so 1 must accept 2, and return a new kind var 100.
--
-- KConstEnv:
-- Either : * -> * -> *
-- ExceptT : 4
--
-- KEnv:
-- m : 0
-- e : 2
--
-- KSubst:
-- 1 : 2 -> 100
--
-- ===== (TVarK "a" (KUnknown 3))
--
-- KConstEnv:
-- Either : * -> * -> *
-- ExceptT : 4
--
-- KEnv:
-- m : 0
-- e : 2
-- a : 3
--
-- KSubst:
-- 1 : 2 -> 100
--
-- ===== (TApp (TApp (TConst "Either" (KUnknown 1)) (TVarK "e" (KUnknown 2)))
--             (TVarK "a" (KUnknown 3)))
--
-- KConstEnv:
-- Either : * -> * -> *
-- ExceptT : 4
--
-- KEnv:
-- m : 0
-- e : 2
-- a : 3
--
-- KSubst:
-- 1 : 2 -> 100
-- 100: 3 -> 200
-- apply ==>
-- 1 : 2 -> 3 -> 200
-- 100: 3 -> 200
--
-- ===== TApp (TVarK "m" (KUnknown 0))
-- =====      (TApp (TApp (TConst "Either" (KUnknown 1)) (TVarK "e" (KUnknown 2)))
-- =====            (TVarK "a" (KUnknown 3)))
--
-- KConstEnv:
-- Either : * -> * -> *
-- ExceptT : 4
--
-- KEnv:
-- m : 0
-- e : 2
-- a : 3
--
-- KSubst:
-- 1 : 2 -> 3 -> 200
-- 100: 3 -> 200
-- 0 : 200 -> 300
--
-- Replace all unknowns with stars:
--
-- KEnv:
-- m : * -> *
-- e : *
-- a : *
--
-- KSubst:
-- 1 : * -> * -> *
-- 100: * -> *
-- 0 : * -> *
--
-- And get:
--
-- ExceptT e m a -> *
-- ExceptT * -> (((* -> *) -> *) -> *)
--
---------------------------
-- Dallas
---------------------------
--
-- Start from:
--
-- data Dallas a b c d e =  Dallas (a (b c) d e)
--
-- In Neblen:
--
-- (data-type Dallas (a b c d e)
--            (Dallas (a (b c) d e)))
--
-- Parse this, and give each TVarK a unique integer:
--
-- dtFoo :: Declare
-- dtFoo = DeclareType "Foo" ["a","b","c","d"]
--           [DeclareCtor "Foo"
--             [TApp (TApp (TApp (TVarK "a" (KUnknown 0))
--                               (TApp (TVarK "b" (KUnknown 1)) (TVarK "c" (KUnknown 2))))
--                         (TVarK "d" (KUnknown 3)))
--                   (TVarK "e" (KUnknown 4))]]
--           (KUnknown 5)
--
-- Start with inner-most LHS type:
--
-- (TVarK "a" (KUnknown 0))
--
-- Known data types mapping:
-- Dallas : 5
--
-- TVarK to kind mapping:
-- a : 0
-- b : 1
-- c : 2
-- d : 3
-- e : 4
--
-- Move to the RHS:
--
-- (TVarK "b" (KUnknown 1))
--
-- a : 0
-- b : 1
-- c : 2
-- d : 3
-- e : 4
--
-- (TVarK "c" (KUnknown 2))
--
-- a : 0
-- b : 1
-- c : 2
-- d : 3
-- e : 4
--
-- (TApp (TVarK "b" (KUnknown 1)) (TVarK "c" (KUnknown 2)))
--
-- Now we're back to the inner-most TApp. Here, we unify and discover new info
-- about b's k1, namely that it must be 2 -> 100, where 100 is a fresh kind var.
--
-- Lazy apply:
-- a : 0
-- b : 1
-- c : 2
-- d : 3
-- e : 4
-- 1 : 2 -> 100
--
-- Eager apply:
--  Note that we don't need to apply the kinds into the type vars yet. 'b' can
--  stay as 'b : 1' until the end. I think all we need to carry forward are the
--  kind mappings (e.g. 1 : 2 -> 100), composing them together at every step.
-- a : 0
-- b : 2 -> 100
-- c : 2
-- d : 3
-- e : 4
-- 1 : 2 -> 100
--
--  (TApp (TVarK "a" (KUnknown 0))
--        (TApp (TVarK "b" (KUnknown 1)) (TVarK "c" (KUnknown 2))))
--
-- Same as before, get a fresh k200. Kind of expression is k200, but we
-- discovered new info about a's k0:
--
-- Lazy apply:
-- a : 0
-- b : 1
-- c : 2
-- d : 3
-- e : 4
-- 1 : 2 -> 100
-- 0 : 100 -> 200
--
-- Eager apply:
-- a : 100 -> 200
-- b : 2 -> 100
-- c : 2
-- d : 3
-- e : 4
-- 1 : 2 -> 100
-- 0 : 100 -> 200
--
-- (TApp (TApp (TVarK "a" (KUnknown 0))
--             (TApp (TVarK "b" (KUnknown 1)) (TVarK "c" (KUnknown 2))))
--       (TVarK "d" (KUnknown 3)))
--
-- Kind of expression is k300.
--
-- Lazy apply:
-- a : 0
-- b : 1
-- c : 2
-- d : 3
-- e : 4
-- 1 : 2 -> 100
-- 0 : 100 -> 200
-- 200: 3 -> 300
--
-- Eager apply:
-- a : 100 -> (3 -> 300)
-- b : 2 -> 100
-- c : 2
-- d : 3
-- e : 4
-- 1 : 2 -> 100
-- 0 : 100 -> (3 -> 300)
-- 200: 3 -> 300
--
-- TApp (TApp (TApp (TVarK "a" (KUnknown 0))
--                  (TApp (TVarK "b" (KUnknown 1)) (TVarK "c" (KUnknown 2))))
--            (TVarK "d" (KUnknown 3)))
--      (TVarK "e" (KUnknown 4))
--
-- Kind of expression is k400.
--
-- Lazy apply:
-- a : 0
-- b : 1
-- c : 2
-- d : 3
-- e : 4
-- 1 : 2 -> 100
-- 0 : 100 -> 200
-- 200: 3 -> 300
-- 300: 4 -> 400
--
-- Eager apply:
-- a : 100 -> (3 -> (4 -> 400))
-- b : 2 -> 100
-- c : 2
-- d : 3
-- e : 4
-- 1 : 2 -> 100
-- 0 : 100 -> (3 -> (4 -> 400))
-- 200: 3 -> (4 -> 400)
-- 300: 4 -> 400
--
-- Then, apply substitutions on k vars and t vars. Or, should we have been
-- applying them as we went along, similar to how type checking works? I wonder
-- if data types being recursive makes it easier to do one way over the other.
--
-- Lazy apply:
-- a : 100 -> (3 -> (4 -> 400))
-- b : 2 -> 100
-- c : 2
-- d : 3
-- e : 4
-- 1 : 2 -> 100
-- 0 : 100 -> (3 -> (4 -> 400))
-- 200: 3 -> (4 -> 400)
-- 300: 4 -> 400
--
-- Eager apply (subsitutions already applied!):
-- a : 100 -> (3 -> (4 -> 400))
-- b : 2 -> 100
-- c : 2
-- d : 3
-- e : 4
-- 1 : 2 -> 100
-- 0 : 100 -> (3 -> (4 -> 400))
-- 200: 3 -> (4 -> 400)
-- 300: 4 -> 400
--
-- To get:
--
-- a : 100 -> (3 -> (4 -> 400))
-- b : 2 -> 100
-- c : 2
-- d : 3
-- e : 4
--
-- Then, replace all unknowns with stars.
--
--  a : * -> (* -> (* -> *))
--  b : * -> *
--  c : *
--  d : *
--  e : *
--
-- And get:
--
-- Dallas : (* -> (* -> (* -> *))) -> (* -> *) -> * -> * -> * -> *

---------------------------
-- Manual evaluation of recursive types
---------------------------
--
-- data Tree a = Leaf a | Branch (Tree a) (Tree a)
--
-- DeclareType "Tree" ["a"]
--             [DeclareCtor "Leaf" [TVarK "a" (KUnknown 0)],
--              DeclareCtor "Branch" [TApp (TConst "Tree" (KUnknown 1)) (TVarK "a" (KUnknown 0)),
--                                    TApp (TConst "Tree" (KUnknown 1)) (TVarK "a" (KUnknown 0))]]
--             (KUnknown 2)
--
-- Starting from most-inner LHS of the first element:
--
-- TVarK "a" (KUnknown 0)
--
-- Data type mapping:
-- Tree : 2
--
-- TVarK to kind mapping:
-- a : 0
--
-- Done with this one. We can't convert 0 to * yet. Must go to the other ctors.
--
-- DeclareCtor "Branch" [TApp (TConst "Tree" (KUnknown 1)) (TVarK "a" (KUnknown 0)),
--                       TApp (TConst "Tree" (KUnknown 1)) (TVarK "a" (KUnknown 0))]]
--
-- (TConst "Tree" (KUnknown 1))
--
-- Look up "Tree", we note that it is being declared. So we just say:
--
-- a : 0
--
-- (TVarK "a" (KUnknown 0))
--
-- a : 0
--
-- TApp (TConst "Tree" (KUnknown 1)) (TVarK "a" (KUnknown 0))
--
-- a : 0
-- 1 : 0 -> 100
--
-- Go the second argument of ctor:
--
-- (TConst "Tree" (KUnknown 1))
--
-- a : 0
-- 1 : 0 -> 100
--
-- (TVarK "a" (KUnknown 0))
--
-- a : 0
-- 1 : 0 -> 100
--
-- TApp (TConst "Tree" (KUnknown 1)) (TVarK "a" (KUnknown 0))
--
-- a : 0
-- 1 : 0 -> 100
--
-- We are done:
--
-- a : 0
--
-- Replace with *:
--
-- a : *
--
-- And as for Tree, we just iterate through its declared variables (just 'a' for
-- this):
--
-- Tree : * -> *
--
-- And return:
--
--
-- dtTree :: Declare
-- dtTree = DeclareType "Tree" ["a"]
--                      [DeclareCtor "Leaf" [TVarK "a" Star],
--                       DeclareCtor "Branch" [TApp (TConst "Tree" (KFun Star Star)) (TVarK "a" Star),
--                                             TApp (TConst "Tree" (KFun Star Star)) (TVarK "a" Star)]]
--                      (KFun Star Star)

-- | Mapping of data type constant to kind. (e.g. Left : KFun KStar KStar)
type KConstEnv = M.Map TName Kind

-- TODO
-- Game plan:
--
-- - Call evalKind for each ctor, passing in the new kenv and ksub every go-round.
-- - At the end, you have a kenv and ksub ready to go. Call a "substitute"
--   method that replaces all of unknowns with stars.
-- - Then, we need to return a new DeclareType with the kinds filled in
evalDataTypeKind :: KConstEnv -> KEnv -> Declare -> (KEnv, Declare)
evalDataTypeKind cenv kenv declare@(DeclareType name tvs ctors kind) = do
  let (kenv2, ksub2) = foldl (\(kenv', ksub') ctor ->
                               let (kenv'', ksub'') = evalKind cenv kenv' ksub' ctor
                               in (kenv'', ksub'' `composeKSubst` ksub'))
                         (kenv, M.empty) ctors
  -- TODO:
  --  - as we are replacing the unknown kind vars with stars, we need to
  --  remember the mapping so that we know the final kind of this data type. But
  --  kapply doesn't return a KEnv. So how?
  (kenv2, kapply ksub2 declare)
evalDataTypeKind cenv kenv _ = error "Should only be called for top-level data type declaration."

-- Figure out what the heck this kind is!
evalKind :: KConstEnv -> KEnv -> KSubst -> Declare -> (KEnv, KSubst)
evalKind cenv kenv ksub (DeclareCtor name types) = (kenv, ksub)
  -- As we go through each type, getting new KEnv and KSubst, apply and merge
  -- these envs together.
evalKind cenv kenv ksub (DeclareType {}) = error "Should not be called on DeclareTypes"

-- | Evaluate kind of type.
--
-- >>> evalKindOfType M.empty M.empty M.empty (TVarK "a" (KUnknown 0))
-- (fromList [("a",k0)],fromList [],k0)
--
-- >>> evalKindOfType (M.fromList [("Foo", KUnknown 0)]) M.empty M.empty (TConst "Foo" (KUnknown 0))
-- (fromList [],fromList [],k0)
--
-- >>> evalKindOfType M.empty M.empty M.empty (TConst "Foo" (KUnknown 0))
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
--   a : 1 -> 100
--   b : 1
--
--   KSubst:
--   0 : 1 -> 100
--
-- >>> evalKindOfType M.empty (M.fromList [("a",KUnknown 0), ("b",KUnknown 1)]) M.empty (TApp (TVarK "a" (KUnknown 0)) (TVarK "b" (KUnknown 1)))
-- (fromList [("a",(k1 -> k100)),("b",k1)],fromList [(0,(k1 -> k100))],k100)
--
-- For below, we have:
--
--   KEnv:
--   a : 0
--   b : 1
--   c : 2
--   d : 3
--   e : 4
--
--   KSubst:
--   empty
--
-- Expected results:
--
--   KEnv:
--   a : 200 -> (3 -> 100)
--   b : 2 -> 200
--   c : 2
--   d : 3
--   e : 4
--
--   KSubst:
--   1 : 2 -> 200
--   0 : 200 -> (3 -> 100)
--   300: 3 -> 100
--
-- >>> evalKindOfType M.empty (M.fromList [("a",KUnknown 0), ("b",KUnknown 1), ("c",KUnknown 2), ("d",KUnknown 3), ("e",KUnknown 4)]) M.empty (TApp (TApp (TVarK "a" (KUnknown 0)) (TApp (TVarK "b" (KUnknown 1)) (TVarK "c" (KUnknown 2)))) (TVarK "d" (KUnknown 3)))
-- ???
--
-- ============
--
evalKindOfType :: KConstEnv -> KEnv -> KSubst -> Type -> (KEnv, KSubst, Kind)
evalKindOfType cenv kenv ksub (TVarK tv (KUnknown kv)) = (kenv, ksub, KUnknown kv)
evalKindOfType cenv kenv ksub (TConst name k) =
  if M.member name cenv
  then (kenv, ksub, k)
  else (error ("Unknown TConst: " ++ show (TConst name k)))

evalKindOfType cenv kenv ksub (TApp t1 t2) =
  let (kenv1, ksub1, k1) = evalKindOfType cenv kenv ksub t1
      (kenv2, ksub2, k2) = evalKindOfType cenv kenv (ksub1 `composeKSubst` ksub) t2
      tv = KUnknown 100
      ksub3 = M.insert (getKindVar k1) (KFun k2 tv) ksub2
  in (M.map (kapply ksub3) kenv2, ksub3, tv)

getKindVar :: Kind -> Int
getKindVar (KUnknown i) = i

--
-- Kind unification:
-- https://github.com/purescript/purescript/blob/master/src/Language/PureScript/TypeChecker/Kinds.hs#L58
--
-- Purescript uses KUnknown with generated integer as "kind variables":
-- https://github.com/purescript/purescript/blob/master/src/Language/PureScript/Kinds.hs#L33
--
-- (=:=): https://github.com/purescript/purescript/blob/master/src/Control/Monad/Unify.hs#L120
--
-- Has idea of converting all unknown kinds to Star at the end:
-- https://github.com/purescript/purescript/blob/master/src/Language/PureScript/TypeChecker/Kinds.hs#L149
--
--
