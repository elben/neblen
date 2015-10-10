module Neblen.DataTypes2 where

import Neblen.Data
import Neblen.DataTypes

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


-- Data types
--
-- Data types can only be declared at the top level.
--
-- ===============
-- Parsing
-- ===============
--
-- First, we need to parse the data type declaration into something that we
-- can work with. When you declare a data type, you're declaring:
--
--   - Its name (e.g. Maybe)
--   - What its constructors are
--     - For each constructor, what types it accepts, and what its kinds are
--
-- Some examples:
--
-- (data-type Person
--            (Person String Int))
--
dtPerson :: DeclareType
dtPerson = DeclareType "Person" [] [DeclareCtor "Person" [TString,TInt]] (KUnknown 0)
--
-- Maybe:
-- (data-type Maybe (a)
--            Nothing
--            (Just a))
--
-- Parser "dumbly" parses this data type declaration. Note that the kinds of
-- each type is currently unknown:
--
dtMaybe :: DeclareType
dtMaybe = DeclareType "Maybe" ["a"] [DeclareCtor "Nothing" [],
                                     DeclareCtor "Just" [TVarK "a" (KUnknown 0)]]
                      (KUnknown 1)
-- Either:
-- (data-type Either (a b)
--            (Left a)
--            (Right b))
--
dtEither :: DeclareType
dtEither = DeclareType "Either" ["a","b"] [DeclareCtor "Left"  [TVarK "a" (KUnknown 0)],
                                           DeclareCtor "Right" [TVarK "b" (KUnknown 1)]]
           (KUnknown 2)

-- (data-type ExceptT (e m a)
--            (ExceptT (m (Either e a))))
--
dtExceptT :: DeclareType
dtExceptT = DeclareType "ExceptT" ["e","m","a"]
                        [DeclareCtor "ExceptT"
                                  [TApp (TVarK "m" (KUnknown 0))
                                        (TApp (TApp (TConst "Either" (KUnknown 1)) (TVarK "e" (KUnknown 2)))
                                              (TVarK "a" (KUnknown 3)))]]
            (KUnknown 4)

-- (data-type Pair (a b)
--   (Pair a b))
--
dtPair :: DeclareType
dtPair = DeclareType "Pair" ["a","b"]
                     [DeclareCtor "Pair" [TVarK "a" (KUnknown 1),TVarK "b" (KUnknown 2)]]
                     (KUnknown 3)

dtPair2 :: DeclareType
dtPair2 = DeclareType "Pair" ["a","b"]
                      [DeclareCtor "Pair" [TVarK "a" Star,TVarK "b" Star]]
                      (KFun Star (KFun Star Star))


-- | Primitive function type.
funT :: Type
funT = (TConst "->" (KFun Star (KFun Star Star)))

-- (data-type State (s a)
--   (State (-> s (Pair s a))))
--
dtState :: DeclareType
dtState = DeclareType "State" ["s","a"]
                      [DeclareCtor "State"
                        [TApp (TApp (TConst "->" (KUnknown 0))
                                    (TVarK "s" (KUnknown 1)))
                              (TApp (TApp (TConst "Pair" (KUnknown 2)) (TVarK "s" (KUnknown 3)))
                                    (TVarK "a" (KUnknown 4)))]]
                      (KUnknown 5)
dtState2 :: DeclareType
dtState2 = DeclareType "State" ["s","a"]
                       [DeclareCtor "State"
                         [TApp (TApp funT
                                     (TVarK "s" Star))
                               (TApp (TApp (TConst "Pair" (KFun Star (KFun Star Star))) (TVarK "s" Star))
                                     (TVarK "a" Star))]]
                       (KFun (KFun Star Star) Star)

-- Recursive data type:
data Tree a = Leaf a
            | Branch (Tree a) (Tree a)

dtTree :: DeclareType
dtTree = DeclareType "Tree" ["a"]
                     [DeclareCtor "Leaf" [TVarK "a" (KUnknown 0)],
                      DeclareCtor "Branch" [TApp (TConst "Tree" (KUnknown 1)) (TVarK "a" (KUnknown 0)),
                                            TApp (TConst "Tree" (KUnknown 1)) (TVarK "a" (KUnknown 0))]]
                     (KUnknown 1)

-- Foo :: (* -> * -> *) -> * -> * -> *
--
data Dallas a b c d e =  Dallas (a (b c) d e)
--
dtFoo :: DeclareType
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
-- dtExceptT :: DeclareType
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
-- dtFoo :: DeclareType
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
-- Tree : 1
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
-- dtTree :: DeclareType
-- dtTree = DeclareType "Tree" ["a"]
--                      [DeclareCtor "Leaf" [TVarK "a" Star],
--                       DeclareCtor "Branch" [TApp (TConst "Tree" (KFun Star Star)) (TVarK "a" Star),
--                                             TApp (TConst "Tree" (KFun Star Star)) (TVarK "a" Star)]]
--                      (KFun Star Star)


-- ===============
-- Kind finder
-- ===============
--
-- Then, we pass this parsed expression into the kind-finder. How the
-- kind-finder works:
--
--   - Recursively evaluate each element, starting with an empty context
--     - Context is map of tvars to its kind
--
--   - If it sees a "standalone" TVarK with unknown kind, set kind to Star
--     - Save this knowledge in a map context
--
--   - If it sees a constant, like "Either", look into a map to see that data
--     type's kind. If doesn't exist, error.
--
--   - If it sees a TApp, evaluate the RHS's kind.
--     - Evaluate the LHS's kind. Unify RHS kind with LHS kind (like function
--       unification in the type level)
--       - If LHS's kind is exactly that, good. Return.
--       - If LHS's kind is unknown, set it to RHS-KIND -> *
--       - If LHS's kind is not RHS-KIND -> *, error.
--
--   - If it sees a TConst, return.
--   - When we pop back off to the DeclareType level, we now know each tvar's kind.
--   - We can now deduce the data type's kind. Maybe we do this somewhere else:
--     - Check that each tvar the data type declares is used.
--     - Find each of their kind, ordered by the data type's tvar declaration order.
--     - Put them together, and you get the data type's kind.
--
dtMaybe2 :: DeclareType
dtMaybe2 = DeclareType "Maybe" ["a"] [DeclareCtor "Nothing" [],
                                      DeclareCtor "Just" [TVarK "a" Star]]
                       (KFun Star Star)
dtEither2 :: DeclareType
dtEither2 = DeclareType "Either" ["a","b"] [DeclareCtor "Left"  [TVarK "a" Star],
                                            DeclareCtor "Right" [TVarK "b" Star]]
                        (KFun (KFun Star Star) Star)
dtExceptT2 :: DeclareType
dtExceptT2 = DeclareType "ExceptT" ["e","m","a"]
                         [DeclareCtor "ExceptT"
                                   [TApp (TVarK "m" (KFun Star Star))
                                         (TApp (TApp (TConst "Either" (KFun Star (KFun Star Star))) (TVarK "e" Star))
                                               (TVarK "a" Star))]]
                         (KFun Star (KFun (KFun Star Star) (KFun Star Star)))

dtPerson2 :: DeclareType
dtPerson2 = DeclareType "Person" [] [DeclareCtor "Person" [TString,TInt]] Star


-- More examples:
data Foo a = Foo (Maybe a)
data Bar a b = Bar (a b)
data Far a b = Far (a b) b
data Complex a b c d e = Complex (b a) (a (c d e))

foo1 :: Foo Int
foo1 = Foo (Just (0 :: Int))

foo2 :: Foo a
foo2 = Foo Nothing

bar1 :: Bar Maybe Int
bar1 = Bar (Just (0 :: Int))

far1 :: Far Maybe Int
far1 = Far (Just (0 :: Int)) 1

-- Complex :: b a -> a (c d e) -> Complex a b c d e
--
-- b = ?
-- a = Maybe
-- c = Either d Int
-- d is free
-- e = Int
--
-- Kind of `Complex` is:
--
--   a :: * -> *
--   b :: (* -> *) -> *
--   c :: * -> * -> *
--   d :: *
--   e :: *
--
-- Clownpiece: So something like Fix f = Fold {unFold :: f (Fix f)} would work for b.
--
-- complex1 :: Complex Maybe ? (Either d Int) d Int
-- complex1 = Complex _ (Just (Right (0 :: Int)))

-- data Fix f = Fold { unFold :: f (Fix f) }

