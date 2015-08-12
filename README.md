A programming language, a typed Lisp, a fun time.

```
Neblen> (let [incr (+ 1)] (incr (incr 10)))
12 : Int

Neblen> (let [twice (fn [f x] (f (f x)))] ((twice (+ 1)) 10))
12 : Int

Neblen> :t (fn [x] x)
(fn [x] x) : (-> a a)

Neblen> :t ((fn [x] x) (fn [y] (y true)))
((fn [x] x) (fn [y] (y true))) : (-> (-> Bool a) a)
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
