module Neblen.DataTypes where

import Neblen.Data
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

-- The current problem:
--
-- Right now, findKindT doesn't work for this:
--
--   data ExceptT e m a = ExceptT (m (Either e a))
--
-- Because findKindT will see the TApp m (Either e a), and assign m's kind to *,
-- even though it should be * -> *.
--
-- I think we may need a kind unification for TApp.

-- A place to play with data types implementation

-- Data type declaration is separated out from regular expressions.
--
data Declare =
             -- DeclareType Name [TName] [DeclareCtor] Kind
               DeclareType Name [TName] [Declare] Kind
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

-- | Mapping of type variable to kind.
type KEnv = M.Map TName Kind

-- | Mapping of unknown kind variables to kinds.
type KSubst = M.Map Int Kind

-- | Find kind.
--
-- >>> findKind (M.empty) (M.empty) (M.empty) dtPerson
-- (fromList [],fromList [],DeclareType "Person" [] [DeclareCtor "Person" [String,Int]] *)
--
-- >>> findKind (M.empty) (M.empty) (M.empty) dtMaybe
-- (fromList [("a",*)],DeclareType "Maybe" ["a"] [DeclareCtor "Nothing" [],DeclareCtor "Just" [a]] (* -> *))
-- (fromList [("a",k0)],fromList [],DeclareType "Maybe" ["a"] [DeclareCtor "Nothing" [],DeclareCtor "Just" [a]] (k0 -> *))
--
-- >>> findKind (M.empty) (M.empty) (M.empty) dtEither
-- (fromList [("a",*),("b",*)],DeclareType "Either" ["a","b"] [DeclareCtor "Left" [a],DeclareCtor "Right" [b]] (* -> (* -> *)))
--
-- >>> findKind (M.fromList [("->",kindOf funT),("Pair",kindOf dtPair2)]) M.empty (M.empty) dtState
-- (fromList [("a",*),("s",*)],DeclareType "State" ["s","a"] [DeclareCtor "State" [(-> ((Pair s) a))]] (* -> (* -> *)))
--
-- >>> findKind (M.fromList [("Either",kindOf dtEither2)]) (M.empty) (M.empty) dtExceptT
-- (fromList [("a",*),("e",*),("m",(* -> *))],DeclareType "ExceptT" ["e","m","a"] [DeclareCtor "ExceptT" [(m ((Either e) a))]] (* -> ((* -> *) -> (* -> *))))
--
findKind ::
  -- Env of known data types.
  KEnv
  -- Env for a data type declaration's type variables.
  -> KEnv
  -> KSubst
  -> Declare
  -> (KEnv, KSubst, Declare)
findKind dtenv kenv ksub (DeclareType name tvs exprs (KUnknown _)) =
  -- Create new kenv for new data types
  --
  -- TODO:
  --
  --   - Each data type declaration needs to be in a "Forall" so that it knows
  --     which tvars it owns, and so that we instantiate fresh ones when we use
  --     the data type
  --
  let kenv = (M.fromList (zip tvs freshKinds))
      (kenv2, ksub2, exprs1) = foldl
                          (\(kenvSum, ksubSum, es) e ->
                            let (kenvSum', ksub', e') = findKind dtenv kenvSum ksubSum e
                            in (kenvSum', ksub', e':es))
                          (kenv, ksub, []) exprs

      -- Find the data type's kind, using its tv-to-kind mapping.
      dataTypeKind = foldr
                       (\tv finalKind ->
                         case M.lookup tv kenv2 of
                           Just kind -> KFun kind finalKind
                           _ -> error "type variable should know kind by now")
                       Star tvs
  in (kenv2, ksub, DeclareType name tvs (reverse exprs1) dataTypeKind)

findKind dtenv kenv ksub (DeclareCtor name types) =
  let (kenv', ksub2, ts1) = foldl
                       (\(kenvSum, ksubSum, ts) t ->
                         let (kenvSum', ksub', t') = findKindT dtenv kenvSum ksubSum t
                         in (kenvSum', ksub', t':ts))
                       (kenv, ksub, []) types
  in (kenv', ksub2, DeclareCtor name (reverse ts1))
findKind _ _ _ _ = error "not a data type declaration thing"

findKindT :: KEnv -> KEnv -> KSubst -> Type -> (KEnv, KSubst, Type)
findKindT _ kenv ksub (TVarK name (KUnknown _)) =
  case M.lookup name kenv of
    Just k            -> (kenv, ksub, TVarK name k)
    _                 -> error ("Type var " ++ name ++ " is not in the data declaration.")
-- Kind known, just return the type.
findKindT _ kenv ksub t@(TVarK {}) = (kenv, ksub, t)

findKindT dtenv kenv ksub (TConst name (KUnknown _)) =
  case M.lookup name dtenv of
    Just k -> (kenv, ksub, TConst name k)
    Nothing  -> error ("unknown type const: " ++ name)
-- Kind known, just return the type.
findKindT _ kenv ksub t@(TConst {}) = (kenv, ksub, t)
findKindT dtenv kenv ksub (TApp t1 t2) =
  let (kenv1, ksub1, t1') = findKindT dtenv kenv ksub t1
      (kenv2, ksub2, t2') = findKindT dtenv kenv1 ksub1 t2
  in (kenv2, ksub2, TApp t1' t2')
findKindT _ kenv ksub TInt = (kenv, ksub, TConst "Int" Star)
findKindT _ kenv ksub TBool = (kenv, ksub, TConst "Bool" Star)
findKindT _ kenv ksub TString = (kenv, ksub, TConst "String" Star)

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

