I don't know what Neblen is.

# Development

```
cabal init sandbox
cabal install --only-dep
```

# Tests

```
doctest -isrc -Wall -fno-warn-type-defaults -fno-warn-unused-do-bind src/Neblen/Parser.hs
```
