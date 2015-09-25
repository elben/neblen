module Neblen.DataTypes where

import Neblen.Data
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.State

-- | Data type declarations. Separated out from regular expressions.
--
data Declare = DeclareType Name [TName] [Declare] Kind
             | DeclareCtor Name [Type]
  deriving (Show, Eq, Ord)

class HasKind a where
  kindOf :: a -> Kind

instance HasKind Declare where
  kindOf (DeclareType _ _ _ k) = k
  kindOf DeclareCtor{} = error "One does not ask for the kind of a constructor."

instance HasKind Type where
  kindOf (TConst _ k) = k
  kindOf (TVarK _ k) = k
  kindOf (TApp t1 _) = case kindOf t1 of
                         (KFun _ k) -> k
  kindOf _ = error "One does not ask for the kind of a constructor."

replaceKind :: Type -> Kind -> Type
replaceKind (TVarK n k) k' = TVarK n k'

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
dtPerson :: Declare
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
dtMaybe :: Declare
dtMaybe = DeclareType "Maybe" ["a"] [DeclareCtor "Nothing" [],
                                     DeclareCtor "Just" [TVarK "a" (KUnknown 0)]]
                      (KUnknown 1)
-- Either:
-- (data-type Either (a b)
--            (Left a)
--            (Right b))
--
dtEither :: Declare
dtEither = DeclareType "Either" ["a","b"] [DeclareCtor "Left"  [TVarK "a" (KUnknown 0)],
                                           DeclareCtor "Right" [TVarK "b" (KUnknown 1)]]
           (KUnknown 2)

-- (data-type ExceptT (e m a)
--            (ExceptT (m (Either e a))))
--
dtExceptT :: Declare
dtExceptT = DeclareType "ExceptT" ["e","m","a"]
                        [DeclareCtor "ExceptT"
                                  [TApp (TVarK "m" (KUnknown 0))
                                        (TApp (TApp (TConst "Either" (KUnknown 1)) (TVarK "e" (KUnknown 2)))
                                              (TVarK "a" (KUnknown 3)))]]
            (KUnknown 4)

-- (data-type Pair (a b)
--   (Pair a b))
--
dtPair :: Declare
dtPair = DeclareType "Pair" ["a","b"]
                     [DeclareCtor "Pair" [TVarK "a" (KUnknown 1),TVarK "b" (KUnknown 2)]]
                     (KUnknown 3)

dtPair2 :: Declare
dtPair2 = DeclareType "Pair" ["a","b"]
                      [DeclareCtor "Pair" [TVarK "a" Star,TVarK "b" Star]]
                      (KFun Star (KFun Star Star))


-- | Primitive function type.
funT :: Type
funT = (TConst "->" (KFun Star (KFun Star Star)))

-- (data-type State (s a)
--   (State (-> s (Pair s a))))
--
dtState :: Declare
dtState = DeclareType "State" ["s","a"]
                      [DeclareCtor "State"
                        [TApp (TApp (TConst "->" (KUnknown 0))
                                    (TVarK "s" (KUnknown 1)))
                              (TApp (TApp (TConst "Pair" (KUnknown 2)) (TVarK "s" (KUnknown 3)))
                                    (TVarK "a" (KUnknown 4)))]]
                      (KUnknown 5)
dtState2 :: Declare
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

-- | Mapping of data type constant to kind. (e.g. Left : KFun KStar KStar)
type KConstEnv = M.Map TName Kind

type KindCheck a = State FreshCounter a

-- TODO
-- Game plan:
--
-- - Call evalKind for each ctor, passing in the new kenv and ksub every go-round.
-- - At the end, you have a kenv and ksub ready to go. Call a "substitute"
--   method that replaces all of unknowns with stars.
-- - Then, we need to return a new DeclareType with the kinds filled in
evalDataTypeKind :: KConstEnv -> KEnv -> Declare -> KindCheck (KEnv, Declare)
evalDataTypeKind cenv kenv declare@(DeclareType name tvs ctors kind) = do
  (kenv2, ksub2) <- foldl (\kindCheck ctor -> do
                               (kenv', ksub') <- kindCheck
                               (kenv'', ksub'') <- evalKind cenv kenv' ksub' ctor
                               return (kenv'', ksub'' `composeKSubst` ksub'))
                         (return (kenv, M.empty)) ctors
  -- TODO:
  --  - as we are replacing the unknown kind vars with stars, we need to
  --  remember the mapping so that we know the final kind of this data type. But
  --  kapply doesn't return a KEnv. So how?
  return (kenv2, kapply ksub2 declare)
evalDataTypeKind cenv kenv _ = error "Should only be called for top-level data type declaration."

-- | Evaluate the kind of a data constructor.
--
evalKind :: KConstEnv -> KEnv -> KSubst -> Declare -> KindCheck (KEnv, KSubst)
evalKind cenv kenv ksub (DeclareCtor name types) = return (kenv, ksub)
  -- As we go through each type, getting new KEnv and KSubst, apply and merge
  -- these envs together.
evalKind cenv kenv ksub (DeclareType {}) = error "Should not be called on DeclareTypes"

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
--   e : 4
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
--   e : 4
--
--   KSubst:
--   0 : 10 -> (3 -> 12)
--   1 : 2 -> 10
--   11: 3 -> 12
--
-- >>> evalState (evalKindOfType M.empty (M.fromList [("a",KUnknown 0), ("b",KUnknown 1), ("c",KUnknown 2), ("d",KUnknown 3), ("e",KUnknown 4)]) M.empty (TApp (TApp (TVarK "a" (KUnknown 0)) (TApp (TVarK "b" (KUnknown 1)) (TVarK "c" (KUnknown 2)))) (TVarK "d" (KUnknown 3)))) (initFreshCounterAt 10)
-- (fromList [("a",(k10 -> (k3 -> k12))),("b",(k2 -> k10)),("c",k2),("d",k3),("e",k4)],fromList [(0,(k10 -> (k3 -> k12))),(1,(k2 -> k10)),(11,(k3 -> k12))],k12)
--
evalKindOfType :: KConstEnv -> KEnv -> KSubst -> Type -> KindCheck (KEnv, KSubst, Kind)
evalKindOfType cenv kenv ksub (TVarK tv (KUnknown kv)) = return (M.insert tv (KUnknown kv) kenv, ksub, KUnknown kv)
evalKindOfType cenv kenv ksub (TConst name k) =
  if M.member name cenv
  then return (kenv, ksub, k)
  else error ("Unknown TConst: " ++ show (TConst name k))

evalKindOfType cenv kenv ksub (TApp t1 t2) = do
  (kenv1, ksub1, k1) <- evalKindOfType cenv kenv ksub t1
  (kenv2, ksub2, k2) <- evalKindOfType cenv kenv1 (ksub1 `composeKSubst` ksub) t2
  kv <- nextFreshCounter >>= return . KUnknown
  let ksub3 = (M.fromList [(getKindVar k1,KFun k2 kv)]) `composeKSubst` ksub2 `composeKSubst` ksub1
  return (M.map (kapply ksub3) kenv2, ksub3 `composeKSubst` ksub3, kv)

evalKindOfType _ kenv ksub TInt = return (kenv, ksub, Star)
evalKindOfType _ kenv ksub TBool = return (kenv, ksub, Star)
evalKindOfType _ kenv ksub TString = return (kenv, ksub, Star)

getKindVar :: Kind -> Int
getKindVar (KUnknown i) = i

nextFreshCounter :: KindCheck Int
nextFreshCounter = do
  s <- get -- Same as: ExceptT (liftM Right get)
  put s{getFreshCounter = getFreshCounter s + 1}
  return $ getFreshCounter s

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
dtMaybe2 :: Declare
dtMaybe2 = DeclareType "Maybe" ["a"] [DeclareCtor "Nothing" [],
                                      DeclareCtor "Just" [TVarK "a" Star]]
                       (KFun Star Star)
dtEither2 :: Declare
dtEither2 = DeclareType "Either" ["a","b"] [DeclareCtor "Left"  [TVarK "a" Star],
                                            DeclareCtor "Right" [TVarK "b" Star]]
                        (KFun (KFun Star Star) Star)
dtExceptT2 :: Declare
dtExceptT2 = DeclareType "ExceptT" ["e","m","a"]
                         [DeclareCtor "ExceptT"
                                   [TApp (TVarK "m" (KFun Star Star))
                                         (TApp (TApp (TConst "Either" (KFun Star (KFun Star Star))) (TVarK "e" Star))
                                               (TVarK "a" Star))]]
                         (KFun Star (KFun (KFun Star Star) (KFun Star Star)))

dtPerson2 :: Declare
dtPerson2 = DeclareType "Person" [] [DeclareCtor "Person" [TString,TInt]] Star

-- TODO can eventually combine as `Substitutable a t`?
class KSubstitutable k where
  kapply :: KSubst -> k -> k

-- | Mapping of type variable (from TVarK) to kind.
type KEnv = M.Map TName Kind

-- | Mapping of unknown kind variables to kinds.
type KSubst = M.Map Int Kind

instance KSubstitutable Kind where
  kapply _ Star = Star
  kapply ksub (KFun k1 k2) = KFun (kapply ksub k1) (kapply ksub k2)
  kapply ksub (KUnknown i) = fromMaybe (KUnknown i) (M.lookup i ksub)

instance KSubstitutable Declare where
  kapply ksub (DeclareType name tvs ctors kind) = DeclareType name tvs (map (kapply ksub) ctors) (kapply ksub kind)
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

-- | Unify types and their kinds. Should this just be in the type unifier?
--
-- Examples:
--
-- (TApp (TConst "Either" KUnknown) (TVarK "e" KUnknown))
unifyKinds :: Type -> Type -> KEnv
unifyKinds (TVarK a ka) (TVarK b kb) =
  let k = uKinds ka kb
  in M.fromList [(a,k),(b,k)]
unifyKinds (TApp l1 r1) (TApp l2 r2) = M.union (unifyKinds l1 l2) (unifyKinds r1 r2)
-- unifyKinds (TConst c kc) (TApp l2 r2) =
unifyKinds t1 t2 = error $ "can't unify kinds of these types: " ++ show t1 ++ " and " ++ show t2

uKinds :: Kind -> Kind -> Kind
uKinds Star Star = Star
uKinds Star (KUnknown i) = Star
uKinds (KUnknown i) Star = Star
uKinds (KFun l1 r1) (KFun l2 r2) = KFun (uKinds l1 r1) (uKinds l2 r2)
uKinds (KUnknown i) (KFun l2 r2) = KFun (uKinds (KUnknown i) l2) (uKinds (KUnknown i) r2)
uKinds (KUnknown i) (KUnknown j) = (KUnknown i)
uKinds k1 k2 = error ("can't unify kind " ++ show k1 ++ " with " ++ show k2)

-- | Supply of fresh unknown kinds.
--
freshKinds :: [Kind]
freshKinds = map (\(f, i) -> f i) (zip (repeat KUnknown) [0..])

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

