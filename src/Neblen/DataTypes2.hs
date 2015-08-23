module Neblen.DataTypes2 where

import Neblen.Data
import Neblen.DataTypes
import qualified Data.Map.Strict as M

-- Clean slate to think about TApp

-- data ExceptT e m a = m (Either e a)
--
dtExceptT :: Declare
dtExceptT = DeclareType "ExceptT" ["e","m","a"]
                        [DeclareCtor "ExceptT"
                          [TApp (TVarK "m" (KUnknown 0))
                                (TApp (TApp (TConst "Either" (KUnknown 1)) (TVarK "e" (KUnknown 2)))
                                      (TVarK "a" (KUnknown 3)))]]
            (KUnknown 4)

-- data State s a = State (-> s (Pair s a))
--
dtState :: Declare
dtState = DeclareType "State" ["s","a"]
                      [DeclareCtor "State"
                        [TApp (TApp (TConst "->" (KUnknown 0))
                                    (TVarK "s" (KUnknown 1)))
                              (TApp (TApp (TConst "Pair" (KUnknown 2)) (TVarK "s" (KUnknown 3)))
                                    (TVarK "a" (KUnknown 4)))]]
                      (KUnknown 5)

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

-- Let's manually do some evaluation:
--
-- TApp (TApp (TApp (TVarK "a" (KUnknown 0))
--                  (TApp (TVarK "b" (KUnknown 1)) (TVarK "c" (KUnknown 2))))
--            (TVarK "d" (KUnknown 3)))
--      (TVarK "e" (KUnknown 4))
--
-- Start with inner-most LHS type:
--
-- (TVarK "a" (KUnknown 0))
--
-- a : 0
--
-- (TApp (TVarK "b" (KUnknown 1)) (TVarK "c" (KUnknown 2)))
--
-- Kind of expression above is some new k100, and we discovered some new info
-- about b's k1:
--
-- a : 0
-- b : 1
-- c : 2
-- 1 : 2 -> 100
--
--  (TApp (TVarK "a" (KUnknown 0))
--        (TApp (TVarK "b" (KUnknown 1)) (TVarK "c" (KUnknown 2))))
--
-- Kind of expression is k200, but we discovered new info about a's k0:
--
-- a : 0
-- b : 1
-- c : 2
-- 1 : 2 -> 100
-- 0 : 100 -> 200
--
-- (TApp (TApp (TVarK "a" (KUnknown 0))
--             (TApp (TVarK "b" (KUnknown 1)) (TVarK "c" (KUnknown 2))))
--       (TVarK "d" (KUnknown 3)))
--
-- Kind of expression is k300.
--
-- a : 0
-- b : 1
-- c : 2
-- 1 : 2 -> 100
-- 0 : 100 -> 200
-- d : 3
-- 200: 3 -> 300
--
-- TApp (TApp (TApp (TVarK "a" (KUnknown 0))
--                  (TApp (TVarK "b" (KUnknown 1)) (TVarK "c" (KUnknown 2))))
--            (TVarK "d" (KUnknown 3)))
--      (TVarK "e" (KUnknown 4))
--
-- Kind of expression is k400.
--
-- a : 0
-- b : 1
-- c : 2
-- 1 : 2 -> 100
-- 0 : 100 -> 200
-- d : 3
-- 200: 3 -> 300
-- e : 4
-- 300: 4 -> 400
--
-- Then, apply substitutions on k vars and t vars:
--
-- a : 100 -> (3 -> (4 -> 400))
-- b : 2 -> 100
-- c : 2
-- 1 : 2 -> 100
-- 0 : 100 -> (3 -> (4 -> 400))
-- d : 3
-- 200: 3 -> (4 -> 400)
-- e : 4
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

-- | Mapping of unknown kind variables to kinds.
type KSubst = M.Map Int Kind

applyKSubst :: KSubst -> Kind -> Kind
applyKSubst s k@(KUnknown i) =
  case M.lookup i s of
    Just k' -> applyKSubst s k'
    Nothing -> k
applyKSubst s (KFun k1 k2) = KFun (applyKSubst s k1) (applyKSubst s k2)
applyKSubst _ k = k


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
