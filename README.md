I don't know what Neblen is.

# Development

```
cabal init sandbox
cabal install --only-dep
```

# Tests

```
doctest -isrc -Wall -fno-warn-type-defaults src/Neblen/Compiler.hs
```
