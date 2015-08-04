I don't know what Neblen is.

# Development

```
cabal init sandbox
cabal install --only-dep
```

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
