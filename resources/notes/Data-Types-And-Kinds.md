# Data Types and Kinds

Neblen supports data types similar to Haskell data types. Here's the `Maybe` data type:

```
(data-type Maybe (a)
  Nothing
  (Just a))
```

To check that uasge of type constructors (e.g. `Just` and `Nothing`) valid, we use a concept called "kinds".

```haskell
data Kind = Star
          | KFun Kind Kind
```

For showing, we use `*` for `Star`.

`Nothing` has the kind `*`, and `Just` has the kind `* -> *`, since `Just` accepts one type `a` and returns the type `Maybe a`. So, `Maybe Int` has the kind `*`.

## Finding kinds

In the actual Nelben implementation of kinds, we need kind variables:

```haskell
data Kind = Star
          | KFun Kind Kind
          | KUnknown Int
```

Data type declarations are represented with `DeclareType`, which contains a list of type constructors, or `DeclareCtor`s:

```haskell
-- | Data type declaration.
data DeclareType = DeclareType Name [TName] [DeclareCtor] Kind

-- | Data type constructor declaration.
data DeclareCtor = DeclareCtor Name [Type]

-- Maybe data type
DeclareType "Maybe" ["a"]
  [DeclareCtor "Nothing" [],
   DeclareCtor "Just" [TVarK "a" (KUnknown 0)]]
  (KUnknown 1)
```

When the program is first parsed, every data type and type constructor has an unknown kind, and so we give it kind variables like `KUnknown 0`. The goal is now to find the kind of each type variable in the type. In doing so, we would be able to build the kind of the data type itself, and also the type constructors.

### General Strategy

The goal is to find the kind of each type in every type constructor. This is similar to an interpreter or type checker—there are terms (type variables like `a` in `(Just a)`), and we go down into each term and try to figure out its kind, building a mapping of type variables to kinds, and also a substitution mapping of kind variables to kinds.

Look at `evalKindOfType` to see how this is done.

The interesting case is `TApp t1 t2`, because once you evaluate the kind of `t1` and `t2`, you then create a third kind `k3` which is returned by the application. All of this gets remembered in the substitution, like this:

```
Environment (KEnv):
t1 : k1
t2 : k2

Substitutions of kind variables (KSubst):
k1 : k2 -> k3
```

Let's follow the example for `ExceptT`:


```haskell
data Either e a = Left e | Right a

data ExceptT e m a = ExceptT m (Either e a)

exceptT = DeclareType "ExceptT" ["e","m","a"]
            [DeclareCtor "ExceptT"
              [TApp (TVarK "m" (KUnknown 0))
                    (TApp (TApp (TConst "Either" (KUnknown 1)) (TVarK "e" (KUnknown 2)))
                          (TVarK "a" (KUnknown 3)))]]
          (KUnknown 4)
```

What is passed into `evalKindOfType` is the term `TApp (TVarK "m" (KUnknown 0)) …` with two maps, `KEnv`, which is the mapping of type vars to kinds, and `KSubst`, which specifies kind variables substitutions:

```
KEnv:
EMPTY

KSubst:
EMPTY
```

The first non-`TApp` term is reached:

```haskell
(TVarK "m" (KUnknown 0))
```

The term `m` is simply added:

```
KEnv:
m : k0

KSubst:
EMPTY
```

The next term reached is:

```haskell
(TConst "Either" (KUnknown 1))
```

We first make sure we know what `Either` is. If we don't, error. Otherwise, assume we don't know it's kind, and let's return the kind variable it has, `k1`.

Next:

```haskell
(TVarK "e" (KUnknown 2))
```

And the env is modified:

```
KEnv:
m : k0
e : k2

KSubst:
EMPTY
```

Now we're at the first `TApp`, of the last two terms above:

```haskell
(TApp (TConst "Either" (KUnknown 1)) (TVarK "e" (KUnknown 2)))
```

Here, we make a new kind variable `k10`, and learn something about `k1`:

```
KEnv:
m : k0
e : k2

KSubst:
k1 : k2 -> k10
```

Next:

```haskell
(TVarK "a" (KUnknown 3))
```

```
KEnv:
m : k0
e : k2
a : k3

KSubst:
k1 : k2 -> k10
```

Next is the next outer `TApp`, combining all the terms above:

```haskell
------ (0)
-- (1)
(TApp (TApp (TConst "Either" (KUnknown 1))
            (TVarK "e" (KUnknown 2)))
      (TVarK "a" (KUnknown 3)))
```

Remember that the inner `TApp` (marked as `(0)`), we create a return kind `k10`. That is the kind of the `(0)` term. The outer `TApp`, then, has a new return kind `k11`. Our enviornment is:

```
KEnv:
m : k0
e : k2
a : k3

KSubst:
k1 : k2 -> k10
k10 : k3 -> k11
```

Notice how we can substitute `k10`. We do just that, and get:

```
KEnv:
m : k0
e : k2
a : k3

KSubst:
k1 : k2 -> k3 -> k11
k10 : k3 -> k11
```

This goes on for every term, until we build a finalize `KSubst` and `KEnv`.

# References

[Typing Haskell in Haskell](https://web.cecs.pdx.edu/~mpj/thih/)

PureScript implementation:
- [Kind unification](https://github.com/purescript/purescript/blob/master/src/Language/PureScript/TypeChecker/Kinds.hs#L58)
- [Kinds](https://github.com/purescript/purescript/blob/master/src/Language/PureScript/Kinds.hs#L33)

# Brain dump

```haskell
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


-- Some examples:
--
-- (data-type Person
--            (Person String Int))
--
person :: DeclareType
person = DeclareType "Person" [] [DeclareCtor "Person" [TString,TInt]] (KUnknown 0)
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
```
