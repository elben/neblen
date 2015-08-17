
# Data types

Algebraic data types, per Haskell.

```
type Name = String
data Maybe a = Just a | Nothing
data Either a b = Left a | Right b
data List a = NilList | (a :. List a)
data Tree a = Branch (Tree a) (Tree a)
            | Leaf a

This is just a type alias
(data-type Name String)

Maybe : * -> *
(data-type (Maybe a)
  (Just a)
  Nothing)

Either : * -> * -> *
(data-type (Either a b)
  (Left a)
  (Right b))

List : * -> *
(data-type (List a)
  NilList
  (SomeList a (List a)))

List : * -> *
(data-type (List a)
  NilList
  (:. a (List a)))

Tree : * -> *
(data-type (Tree a)
  (Branch (Tree a) (Tree a))
  (Leaf a))
```


What about named records? Meh, ignore for now.

```
data Person = { name :: String, age :: Int }

data Either a b = Left { left :: a }
                | Right { right :: a }

data List a = NilList
            | SomeList { getHead :: a, getRest :: List a }
  deriving Show
```

## References


# Type annotations

```
(: x Int)
(def x 1)
=> 1

(def y : String "Hello world")
=> "Hello world"

((fn [z] (+ z x)) 10)
=> 11

(: incr (-> Int Int))
(def incr (fn [x] (+ x 1)))

(incr 10)
=> 11

(def decr (fn [x] (- x 1)))
=> TYPE ERROR. x is any, but expected to be Int.
```

## References

https://wiki.haskell.org/Type

https://en.wikibooks.org/wiki/Haskell/More_on_datatypes

http://elm-lang.org/docs/records

http://docs.racket-lang.org/reference/define-struct.html

# Ideas

- Statically-typed lisp.
- Interperted?
- LLVM?
- Compiled to JS?
  - See these compilers for inspiration:
    - [ClojureScript](https://github.com/clojure/clojurescript/blob/master/src/main/clojure/cljs/compiler.cljc)

