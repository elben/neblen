A programming language, a typed Lisp, a fun time.

```
Neblen> (if true 100 200)
100 : Int

Neblen> (if true 100 "false!")
type mismatch: expecting Int but got String

Neblen> ((fn [x y] x) true)
(fn [y] true) : (-> a Bool)

Neblen> :t (fn [x] x)
(-> a a)

Neblen> :t ((fn [x] x) (fn [y] (y true)))
(-> (-> Bool a) a)
```

# Development

```
cd ~/code/neblen

cabal init sandbox
cabal install --only-dep
```

In `ghci`, run `main` to run the REPL. Try the snippets above.

# Tests

```
cabal test

doctest -isrc -Wall -fno-warn-type-defaults -fno-warn-unused-do-bind src/Neblen/Parser.hs
```

# References

[Write You a Haskell by Stephen Diehl](http://dev.stephendiehl.com/fun)

[Types and Programming Languages by Benjamin C. Pierce](https://mitpress.mit.edu/books/types-and-programming-languages)

[Anatomy of Programming Languages by William R. Cook](http://www.cs.utexas.edu/~wcook/anatomy/anatomy.htm)

[The Typed Racket Guide](http://docs.racket-lang.org/ts-guide/)

[Typing Haskell in Haskell](http://web.cecs.pdx.edu/~mpj/thih/TypingHaskellInHaskell.html#sec-atimonad)
