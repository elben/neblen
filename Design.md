
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

# Ideas

- Statically-typed lisp.
- Interperted?
- LLVM?
- Compiled to JS?
  - See these compilers for inspiration:
    - [ClojureScript](https://github.com/clojure/clojurescript/blob/master/src/main/clojure/cljs/compiler.cljc)
