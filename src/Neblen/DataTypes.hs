module Neblen.DataTypes where

import Neblen.Data
import qualified Data.Map.Strict as M

-- A place to play with data types implementation

-- Data type declaration is separated out from regular expressions.
--
data Declare =
             -- DeclareType Name [TName] [DeclareCtor] Kind
               DeclareType Name [TName] [Declare] Kind
             | DeclareCtor Name [Type]
  deriving (Show, Eq, Ord)

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

-- (data-type Pair (a b) (Pair a b))
--
dtPair :: Declare
dtPair = DeclareType "Pair" ["a","b"]
                     [DeclareCtor "Pair" [TVarK "a" KUnknown,TVarK "b" KUnknown]]
                     KUnknown

-- How to do function types like this:
--
-- (data-type State (s a) (State (-> s (Pair s a))))
--
-- Implies that we have (->) as a constructor? Like:
--
--   TApp (->) ...
--
-- dtState :: Declare
-- dtState = DeclareType "State" ["s","a"]
--                      [DeclareCtor "State" [????]]
--                      KUnknown

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
dtExcept2 :: Declare
dtExcept2 = DeclareType "Either" ["a","b"] [DeclareCtor "Left"  [TVarK "a" Star],
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

dtPersonTest :: Bool
dtPersonTest = (M.empty, dtPersonAnswer) == findKind (M.empty) dtPerson

type KEnv = M.Map Name Kind

-- | Find kind.
--
-- >>> findKind (M.empty) dtPerson
-- (fromList [],DeclareType "Person" [] [DeclareCtor "Person" [String<*>,Int<*>]] *)
--
-- >>> findKind (M.empty) dtMaybe
-- (fromList [("a",*)],DeclareType "Maybe" ["a"] [DeclareCtor "Nothing" [],DeclareCtor "Just" [a<*>]] (* -> *))
--
-- >>> findKind (M.empty) dtEither
-- (fromList [("a",*),("b",*)],DeclareType "Either" ["a","b"] [DeclareCtor "Left" [a<*>],DeclareCtor "Right" [b<*>]] (* -> (* -> *)))
--
-- >>> findKind (M.empty) dtExceptT
-- (fromList [("e",*),("m",(* -> *)),("a",*)],DeclareType "ExceptT" ["e","m","a"] [DeclareCtor "ExceptT" [(m<(* -> *)> ((Either<(* -> (* -> *))> e<*>) a<*>))]] (* -> ((* -> *) -> (* -> *))))
--
findKind :: KEnv -> Declare -> (KEnv, Declare)
findKind _ (DeclareType name tvs exprs KUnknown) =
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
                            let (kenvSum', e') = findKind kenvSum e
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

findKind kenv (DeclareCtor name types) =
  let (kenv', ts1) = foldl
                       (\(kenvSum, ts) t ->
                         let (kenvSum', t') = findKindT kenvSum t
                         in (kenvSum', t':ts))
                       (kenv, []) types
  in (kenv', DeclareCtor name (reverse ts1))
findKind _ _ = error "not a data type declaration thing"

findKindT :: KEnv -> Type -> (KEnv, Type)
findKindT kenv (TVarK name KUnknown) =
  case M.lookup name kenv of
    Just KUnknown -> (M.insert name Star kenv, TVarK name Star)
    Just k        -> (kenv, TVarK name k)
    _             -> error ("Type var " ++ name ++ " is not in the data declaration.")
-- TODO need to have a context mapping of data types to do this one!
findKindT kenv (TConst name KUnknown) = (kenv, TConst name KUnknown)
findKindT kenv (TApp t1 t2) = (kenv, TApp t1 t2)

findKindT kenv TInt = (kenv, TConst "Int" Star)
findKindT kenv TBool = (kenv, TConst "Bool" Star)
findKindT kenv TString = (kenv, TConst "String" Star)
