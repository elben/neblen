module Neblen.TypeChecker.Tests where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import qualified Text.ParserCombinators.Parsec as P
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Arrow (second)
import qualified Data.Map.Strict as M

import Neblen.Data
import Neblen.TypeChecker
import Neblen.Parser

tests :: Test
tests = testGroup "Neblen.TypeChecker.Tests" $ concat
    [
      testUnify
    , testCheckLit
    , testCheckVar
    , testCheckLet
    , testCheckNullFun
    , testCheckFun
    , testCheckNullApp
    , testCheckUnaryApp
    , testCheckList
    , testCheckIf
    ]

testUnify :: [Test]
testUnify =
  [
    "Int"  <=> "Int" ==> []
  , "Int"  <=> "a"   ==> [("a","Int")]
  , "Bool" <!> "Int" ==> Mismatch TBool TInt

  , "(-> Int Int)" <=> "(-> a b)" ==> [("a","Int"),("b","Int")]
  , "(-> Int a)"   <=> "(-> a b)" ==> [("a","Int"),("b","Int")]
  , "(-> Int a)"   <=> "(-> b b)" ==> [("a","Int"),("b","Int")]

  , "(-> a b)" <=> "(-> a b)" ==> []
  , "(-> a b)" <=> "(-> b d)" ==> [("a","d"),("b","d")]
  , "(-> a b)" <=> "(-> c d)" ==> [("a","c"),("b","d")]

  , "(-> a b c d)"   <=> "(-> x y b a)" ==> [("a","x"),("b","y"),("c","y"),("d","x")]
  , "(-> a b c d e)" <=> "(-> x y b a)" ==> [("a","(-> d e)"),("b","y"),("c","y"),("x","(-> d e)")]
  , "(-> a b c d)"   <=> "(-> x y)" ==> [("a","x"),("y","(-> b c d)")]
  , "(-> a b c Int)" <=> "(-> x y)" ==> [("a","x"),("y","(-> b c Int)")]
  , "(-> a b c Int)" <!> "(-> x y z Bool)" ==> Mismatch TInt TBool
  , "(-> a b c)"     <!> "(-> x Int)"      ==> Mismatch (TFun [TVar "b",TVar "c"]) TInt

  -- Nullary functions.
  , "(-> a)" <=> "(-> x)"   ==> [("a","x")]
  , "(-> a)" <=> "(-> Int)" ==> [("a","Int")]
  , "(-> Int)" <!> "Int"      ==> Mismatch (TFun [TInt]) TInt
  , "(-> Int)" <!> "(-> a b)" ==> FunctionExpected TInt

  , "a" <!> "(-> a Int)" ==> InfiniteType (TVar "a") (TFun [TVar "a",TInt])
  ]

testCheckLit :: [Test]
testCheckLit =
  [
    "0" =~> "Int"
  , "true" =~> "Bool"
  , "\"\"" =~> "String"
  ]

testCheckVar :: [Test]
testCheckVar =
  [
    "x" ~~> "Int" `withEnv` [("x","Int")]
  , "x" =!> UnboundVariable "x"
  ]

testCheckLet :: [Test]
testCheckLet =
  [
    "(let [x 0] x)"    =~> "Int"
  , "(let [x 0] true)" =~> "Bool"
  , "(let [x (fn [a] a)] (let [y (fn [b c] (b (x 0)))] (let [z (y (fn [z] z))] x)))" =~> "(-> a a)"
  , "(let [x (fn [a] a)] (let [y (fn [b c] (b (x 0)))] (let [z (y (fn [z] z))] y)))" =~> "(-> (-> Int a) b a)"
  , "(let [x (fn [a] a)] (let [y (fn [b c] (b (x 0)))] (let [z (y (fn [z] z))] z)))" =~> "(-> a Int)"
  , "(let [x 0] b)" ~~> "Bool" `withEnv` [("b","Bool")]

  -- let-polymorphism should look up existing type of 'f' when infering 'g'.
  , "((fn [f] (let [g f] (g 0))) (fn [x] true))" =~> "Bool"

  -- let-polymorphism on id function
  , "(let [id (fn [x] x) y (id 3) z (id true)] z)" =~> "Bool"
  , "(let [id (fn [x] x) y (id 3) z (id true)] y)" =~> "Int"
  , "(let [twice (fn [f x] (f (f x))) a (twice (fn [x] 10) 1) b (twice (fn [x] true) false)] a)" =~> "Int"
  , "(let [twice (fn [f x] (f (f x))) a (twice (fn [x] 10) 1) b (twice (fn [x] true) false)] b)" =~> "Bool"
  , "(let [twice (fn [f x] (f (f x))) a (twice (fn [x] 10) true)] a)" =!> Mismatch TInt TBool

  -- rank 3 let-polymorphism (@id@ and @g@ both used with two different types)
  , "(let [id (fn [x] x) g (fn [f x] (f x)) u (g id 3) v (g id true)] v)" =~> "Bool"

  -- Checks that let-polymorphism doesn't generalize variables that are bound.
  -- Pierce pg 334.
  , "((fn [f] (let [g f] (g 0))) (fn [x] (if x x x)))" =!> Mismatch TBool (TVar "c")
  ]

testCheckNullFun :: [Test]
testCheckNullFun =
  [
    "(fn [] 0)" =~> "(-> Int)"
  , "(fn [] (fn [] 0))" =~> "(-> (-> Int))"
  , "(fn [] x)" =!> UnboundVariable "x"
  ]

testCheckFun :: [Test]
testCheckFun =
  [
    "(fn [x] (fn [y] 0))" =~> "(-> a (-> b Int))"
  , "(fn [x] (fn [y] x))" =~> "(-> a (-> b a))"
  , "(fn [x] (x 3))" =~> "(-> (-> Int a) a)"
  , "(fn [x] (let [z (fn [y] (x 3))] x))" =~> "(-> (-> Int a) (-> Int a))"
  , "(fn [f] (fn [x] (f x)))" =~> "(-> (-> a b) (-> a b))"
  , "(fn [f g h] (fn [x y] (f (g (h (y x))))))" =~> "(-> (-> a b) (-> c a) (-> d c) (-> e (-> e d) b))"

  -- Argument should not override outside scope
  , "((fn [y] (fn [x] y)) x)" ~~> "(-> a Bool)" `withEnv` [("x","Bool")]

  -- The "double" function from Pierce (pg 333)
  , "(fn [f] (fn [x] (f (f x))))" =~> "(-> (-> a a) (-> a a))"

  , "(fn [x] (x x))" =!> InfiniteType (TVar "a") (TFun [TVar "a",TVar "b"])
  ]

testCheckNullApp :: [Test]
testCheckNullApp =
  [
    "(x)" ~~> "Bool" `withEnv` [("x","(-> Bool)")]
  , "((fn [] true))" =~> "Bool"
  , "(let [f (fn [] 0)] ((fn [f] (f)) f))" =~> "Int"
  , "(let [x true] (x))" =!> FunctionExpected TBool
  , "(x)" =!> UnboundVariable "x"
  ]

testCheckUnaryApp :: [Test]
testCheckUnaryApp =
  [
    "(x 0)" ~~> "Bool" `withEnv` [("x","(-> Int Bool)")]
  , "(let [x true] (f x))" ~~> "Bool" `withEnv` [("f","(-> Bool Bool)")]
  , "((fn [x] x) f)" ~~> "(-> Int Bool)" `withEnv` [("f","(-> Int Bool)")]
  , "((fn [x] (fn [y] x)) 0)" =~> "(-> a Int)"
  , "(((fn [x] (fn [y] x)) 0) true)" =~> "Int"
  , "((fn [x] (x 3)) (fn [x] x))" =~> "Int"
  , "(let [x (fn [y] y)] (x x))" =~> "(-> a a)"
  , "((fn [y] (y 3)) x)" ~~> "Int" `withEnv` [("x","(-> z z)")]

  , "(let [x (fn [y] (+ 1 y))] (x 3))" =~> "Int"
  , "(let [x (fn [y] (* 2 y))] (x 3))" =~> "Int"
  , "(let [x (fn [y] (and y true))] (x true))" =~> "Bool"
  , "(let [x (fn [y] (or y true))] (x true))" =~> "Bool"
  , "(let [x (fn [y] (xor y true))] (x true))" =~> "Bool"

  , "(x 0)" =!> UnboundVariable "x"
  , "(x 0)" ~!> FunctionExpected TInt `withEnv` [("x","Int")]
  , "(x 0)" ~!> Mismatch TBool TInt `withEnv` [("x","(-> Bool Int)")]
  ]

testCheckList :: [Test]
testCheckList =
  [
    "[]" =~> "[a]"
  , "[0]" =~> "[Int]"
  , "[0 ((fn [x] x) 0) 123]" =~> "[Int]"
  , "[0 true]" =!> Mismatch TInt TBool
  ]

testCheckIf :: [Test]
testCheckIf =
  [
    "(if ((fn [x] true) 0) \"truth\" \"false\")" =~> "String"
  , "(if true (fn [x] x) (fn [y] y))" =~> "(-> a a)"
  , "(if false 0 false)" =!> Mismatch TInt TBool
  ]

----------------------
-- Utility functions
----------------------

toType :: String -> Type
toType s =
  case P.parse parseType "" s of
    Right t -> t
    Left _ -> error ("Bad test case. Could not parse type: " ++ show s)

toExp :: String -> Exp
toExp s =
  case P.parse parseExp "" s of
    Right t -> t
    Left _ -> error ("Bad test case. Could not parse expression: " ++ show s)

toSubst :: [(TName,String)] -> Subst
toSubst pairs = M.fromList (map (second toType) pairs)

-- | Create unify test with expected Subst.
(<=>) ::
  String               -- First type
  -> String            -- Second type
  -> [(TName, String)] -- Expected Subst
  -> Test
(<=>) a b pairs =
  testCase ("unify: " ++ a ++ " <=> " ++ b) $
  run (unify (toType a) (toType b))
  @?= toSubst pairs

-- | Create unify test with expected TypeError.
(<!>) ::
  String       -- First type
  -> String    -- Second type
  -> TypeError -- Expected error
  -> Test
(<!>) a b e =
  testCase ("unify: " ++ a ++ " <=> " ++ b) $
  expectE (unify (toType a) (toType b))
  @?= e

-- | Helper method to make tests look nicer.
(==>) :: a -> a
(==>) = id

(~~>) :: String -> String -> [(Name,String)] -> Test
(~~>) expr t tenv =
  testCase ("check: " ++ expr ++ " : " ++ t) $
  run (checkTypeWith (toExp expr) (M.fromList (map (second (toScheme . toType)) tenv)))
  @?= toType t

(~!>) ::
  String
  -> TypeError
  -> [(Name,String)]
  -> Test
(~!>) expr err tenv =
  testCase ("check to error: " ++ expr) $
  expectE (check (M.fromList (map (second (toScheme . toType)) tenv)) emptySubst (toExp expr))
  @?= err

withEnv :: a -> a
withEnv = id

(=~>) ::
  String
  -> String
  -> Test
(=~>) expr t =
  testCase ("check: " ++ expr ++ " : " ++ t) $
  -- snd (run (check emptyTEnv emptySubst (toExp expr)))
  run (checkType (toExp expr))
  @?= toType t

(=!>) ::
  String
  -> TypeError
  -> Test
(=!>) expr err =
  testCase ("check to error: " ++ expr) $
  expectE (check emptyTEnv emptySubst (toExp expr))
  @?= err

----------------------
-- Runners
----------------------

run :: ExceptT TypeError (State FreshCounter) a -> a
run u = case evalState (runExceptT u) initFreshCounter of
          Left e -> error $ show e
          Right r -> r

expectE :: Show a => ExceptT TypeError (State FreshCounter) a -> TypeError
expectE u = case evalState (runExceptT u) initFreshCounter of
          Left e -> e
          Right r -> error $ "Expected error but got: " ++ show r

