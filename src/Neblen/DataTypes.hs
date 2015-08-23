module Neblen.DataTypes where

import Neblen.Data
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

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
  kindOf (TApp t1 t2) = KFun (kindOf t1) (kindOf t2)
  kindOf _ = error "One does not ask for the kind of a constructor."

class KindSubstitutable t where
  kapply :: KEnv -> t -> t

instance KindSubstitutable Type where
  kapply e (TVarK tv _) = TVarK tv (fromMaybe KUnknown (M.lookup tv e))
  kapply e (TApp t1 t2) = TApp (kapply e t1) (kapply e t2)
  kapply _ t = t

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
dtPerson = DeclareType "Person" [] [DeclareCtor "Person" [TString,TInt]] KUnknown
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
                                     DeclareCtor "Just" [TVarK "a" KUnknown]]
                      KUnknown
-- Either:
-- (data-type Either (a b)
--            (Left a)
--            (Right b))
--
dtEither :: Declare
dtEither = DeclareType "Either" ["a","b"] [DeclareCtor "Left"  [TVarK "a" KUnknown],
                                           DeclareCtor "Right" [TVarK "b" KUnknown]]
           KUnknown

-- (data-type ExceptT (e m a)
--            (ExceptT (m (Either e a))))
--
dtExceptT :: Declare
dtExceptT = DeclareType "ExceptT" ["e","m","a"]
                        [DeclareCtor "ExceptT"
                                  [TApp (TVarK "m" KUnknown)
                                        (TApp (TApp (TConst "Either" KUnknown) (TVarK "e" KUnknown))
                                              (TVarK "a" KUnknown))]]
            KUnknown

-- (data-type Pair (a b)
--   (Pair a b))
--
dtPair :: Declare
dtPair = DeclareType "Pair" ["a","b"]
                     [DeclareCtor "Pair" [TVarK "a" KUnknown,TVarK "b" KUnknown]]
                     KUnknown

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
                      [DeclareCtor "State" [TApp (TConst "->" KUnknown) (TApp (TApp (TConst "Pair" KUnknown) (TVarK "s" KUnknown)) (TVarK "a" KUnknown))]]
                      KUnknown

dtState2 :: Declare
dtState2 = DeclareType "State" ["s","a"]
                       [DeclareCtor "State" [TApp funT (TApp (TApp (TConst "Pair" (KFun Star (KFun Star Star))) (TVarK "s" Star)) (TVarK "a" Star))]]
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

dtPersonAnswer :: Declare
dtPersonAnswer = DeclareType "Person" [] [DeclareCtor "Person" [TString,TInt]] Star

-- | Mapping of type variable to kind.
type KEnv = M.Map TName Kind

-- | Find kind.
--
-- >>> findKind (M.empty) (M.empty) dtPerson
-- (fromList [],DeclareType "Person" [] [DeclareCtor "Person" [String,Int]] *)
--
-- >>> findKind (M.empty) (M.empty) dtMaybe
-- (fromList [("a",*)],DeclareType "Maybe" ["a"] [DeclareCtor "Nothing" [],DeclareCtor "Just" [a]] (* -> *))
--
-- >>> findKind (M.empty) (M.empty) dtEither
-- (fromList [("a",*),("b",*)],DeclareType "Either" ["a","b"] [DeclareCtor "Left" [a],DeclareCtor "Right" [b]] (* -> (* -> *)))
--
-- >>> findKind (M.fromList [("->",kindOf funT),("Pair",kindOf dtPair2)]) M.empty dtState
-- (fromList [("a",*),("s",*)],DeclareType "State" ["s","a"] [DeclareCtor "State" [(-> ((Pair s) a))]] (* -> (* -> *)))
--
-- >>> findKind (M.fromList [("Either",kindOf dtEither2)]) (M.empty) dtExceptT
-- (fromList [("a",*),("e",*),("m",(* -> *))],DeclareType "ExceptT" ["e","m","a"] [DeclareCtor "ExceptT" [(m ((Either e) a))]] (* -> ((* -> *) -> (* -> *))))
--
findKind ::
  -- | Env of known data types.
  KEnv
  -- | Env for a data type declaration's type variables.
  -> KEnv
  -> Declare
  -> (KEnv, Declare)
findKind dtenv _ (DeclareType name tvs exprs KUnknown) =
  -- Create new kenv for new data types
  --
  -- TODO:
  --
  --   - Each data type declaration needs to be in a "Forall" so that it knows
  --     which tvars it owns, and so that we instantiate fresh ones when we use
  --     the data type
  --
  --   - Need to find final Kind of data type
  let kenv = (M.fromList (zip tvs (repeat KUnknown)))
      (kenv2, exprs1) = foldl
                          (\(kenvSum, es) e ->
                            let (kenvSum', e') = findKind dtenv kenvSum e
                            in (kenvSum', e':es))
                          (kenv, []) exprs

      -- Find the data type's kind, using its tv-to-kind mapping.
      dataTypeKind = foldr
                       (\tv finalKind ->
                         case M.lookup tv kenv2 of
                           Just kind -> KFun kind finalKind
                           _ -> error "type variable should know kind by now")
                       Star tvs
  in (kenv2, DeclareType name tvs (reverse exprs1) dataTypeKind)

findKind dtenv kenv (DeclareCtor name types) =
  let (kenv', ts1) = foldl
                       (\(kenvSum, ts) t ->
                         let (kenvSum', t') = findKindT dtenv kenvSum t
                         in (kenvSum', t':ts))
                       (kenv, []) types
  in (kenv', DeclareCtor name (reverse ts1))
findKind _ _ _ = error "not a data type declaration thing"

findKindT :: KEnv -> KEnv -> Type -> (KEnv, Type)
findKindT _ kenv (TVarK name KUnknown) =
  case M.lookup name kenv of
    Just KUnknown -> (M.insert name Star kenv, TVarK name Star)
    Just k        -> (kenv, TVarK name k)
    _             -> error ("Type var " ++ name ++ " is not in the data declaration.")
-- Kind known, just return the type.
findKindT _ kenv t@(TVarK {}) = (kenv, t)

findKindT dtenv kenv (TConst name KUnknown) =
  case M.lookup name dtenv of
    Just k -> (kenv, TConst name k)
    Nothing  -> error ("unknown type const: " ++ name)
-- Kind known, just return the type.
findKindT dtenv kenv t@(TConst {}) = (kenv, t)

-- TODO
--
--   - If it sees a TApp, evaluate the RHS's kind.
--     - Evaluate the LHS's kind. Unify RHS kind with LHS kind (like function
--       unification in the type level)
--       - If LHS's kind is exactly that, good. Return.
--       - If LHS's kind is unknown, set it to RHS-KIND -> *
--       - If LHS's kind is not RHS-KIND -> *, error.
--
-- Example:
--
-- dtExceptT :: Declare
-- dtExceptT = DeclareType "ExceptT" ["e","m","a"]
--                         [DeclareCtor "ExceptT"
--                                   [TApp (TVarK "m" KUnknown)
--                                         (TApp (TApp (TConst "Either" KUnknown) (TVarK "e" KUnknown))
--                                               (TVarK "a" KUnknown))]]
--   Need to discover that m : * -> *.
--
findKindT dtenv kenv (TApp t1 t2) =
  let (kenv1, t1') = findKindT dtenv kenv t1
      (kenv2, t2') = findKindT dtenv kenv1 t2
  in (kenv2, TApp t1' t2')
  -- let (kenv1, t1') = findKindT dtenv kenv t1
  --     (kenv2, t2') = findKindT dtenv kenv1 t2
  --     kenv3        = unifyKinds t1' (TApp (TVarK "newvar" KUnknown) t2')
  -- -- TODO do I need to apply kenv and combine them with previous kenvs like
  -- -- Subst in type checking?
  -- in (kenv3, TApp (kapply kenv3 t1') (kapply kenv3 t2'))

findKindT _ kenv TInt = (kenv, TConst "Int" Star)
findKindT _ kenv TBool = (kenv, TConst "Bool" Star)
findKindT _ kenv TString = (kenv, TConst "String" Star)

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
uKinds Star KUnknown = Star
uKinds KUnknown Star = Star
uKinds (KFun l1 r1) (KFun l2 r2) = KFun (uKinds l1 r1) (uKinds l2 r2)
uKinds KUnknown (KFun l2 r2) = KFun (uKinds KUnknown l2) (uKinds KUnknown r2)
uKinds KUnknown KUnknown = KUnknown
uKinds k1 k2 = error ("can't unify kind " ++ show k1 ++ " with " ++ show k2)

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

