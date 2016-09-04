# Evaluation

At it's core, Neblen is just the lambda calculus. This makes evaluation fairly
simple.

The evaluator is in `Eval.hs`, via the main function `eval'`:

```haskell
eval' :: EvalEnv -> Exp -> Exp
```

This function takes an evaluation environment (mapping of variables to its
expression value), an expression, and returns the reduced expression. There are
hard-coded primitive functions, like binary and numeric operations, and also
`print`.
