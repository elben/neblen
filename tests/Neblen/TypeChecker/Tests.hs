module Neblen.TypeChecker.Tests where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import qualified Data.Map.Strict as M

import Neblen.Data
import Neblen.TypeChecker

tests :: Test
tests = testGroup "Neblen.TypeChecker.Tests" $ concat
    [
      testUnifier
    , testCheckLit
    , testCheckVar
    , testCheckLet
    , testCheckNullFun
    , testcheckFun
    , testCheckNullApp
    , testCheckUnaryApp
    , testCheckList
    , testCheckIf
    ]

testUnifier :: [Test]
testUnifier =
  [
    testCase "unify: Int <=> Int" $
    run (unify emptyUEnv TInt TInt)
    @?= (M.fromList [],TInt)

  , testCase "unify: Int <=> a" $
    run (unify emptyUEnv TInt (TVar "a"))
    @?= (M.fromList [("a",TInt)],TInt)

  , testCase "unify: a <=> b" $
    run (unify (M.fromList [("a",TInt)]) (TVar "a") (TVar "b"))
    @?= (M.fromList [("a",TInt),("b",TInt)],TInt)

  , testCase "unify: (-> Int Int) <=> (-> a b)" $
    run (unify emptyUEnv (TFun TInt TInt) (TFun (TVar "a") (TVar "b")))
    @?= (M.fromList [("a",TInt),("b",TInt)],TFun TInt TInt)

  , testCase "unify: (-> Int a) <=> (-> a b)" $
    run (unify emptyUEnv (TFun TInt (TVar "a")) (TFun (TVar "a") (TVar "b")))
    @?= (M.fromList [("a",TInt),("b",TInt)],TFun TInt TInt)

  , testCase "unify: (-> Int a) <=> (-> b b)" $
    run (unify emptyUEnv (TFun TInt (TVar "a")) (TFun (TVar "b") (TVar "b")))
    @?= (M.fromList [("a",TInt),("b",TInt)],TFun TInt TInt)

  , testCase "unify: {b : Bool}: (-> a a) <=> (-> b b)" $
    run (unify (M.fromList [("b",TBool)]) (TFun (TVar "a") (TVar "a")) (TFun (TVar "a") (TVar "b")))
    @?= (M.fromList [("a",TBool),("b",TBool)],TFun TBool TBool)

  , testCase "unify: {a -> Int}: (-> a b) <=> (-> a b)" $
    run (unify (M.fromList [("a",TInt)]) (TFun (TVar "a") (TVar "b")) (TFun (TVar "a") (TVar "b")))
    @?= (M.fromList [("a",TInt)],TFun TInt (TVar "b"))

  , testCase "unify: (-> a a) <=> (-> b b)" $
    run (unify (M.fromList []) (TFun (TVar "a") (TVar "a")) (TFun (TVar "b") (TVar "b")))
    @?= (M.fromList [("b",TVar "a")],TFun (TVar "a") (TVar "a"))

  , testCase "unify: {b : Bool}: (-> a b) <=> (-> c d) ===> (-> c Bool)" $
    run (unify (M.fromList [("b",TBool)]) (TFun (TVar "a") (TVar "b")) (TFun (TVar "c") (TVar "d")))
    @?= (M.fromList [("b",TBool),("c",TVar "a"),("d",TBool)],TFun (TVar "a") TBool)

  , testCase "unify: (-> a a) <=> (-> a a)" $
    run (unify emptyUEnv (TFun (TVar "a") (TVar "a")) (TFun (TVar "a") (TVar "a")))
    @?= (M.fromList [],TFun (TVar "a") (TVar "a"))

  , testCase "unify: a <=> Int" $
    expectE (unify (M.fromList [("a",TBool)]) (TVar "a") TInt)
    @?= Mismatch TBool TInt

  , testCase "unify: a <=> (-> a Int)" $
    expectE (unify emptyUEnv (TVar "a") (TFun (TVar "a") TInt))
    @?= InfiniteType (TVar "a") (TFun (TVar "a") TInt)
  ]

run :: ExceptT TypeError (State FreshCounter) a -> a
run u = case evalState (runExceptT u) initFreshCounter of
          Left e -> error $ show e
          Right r -> r

expectE :: Show a => ExceptT TypeError (State FreshCounter) a -> TypeError
expectE u = case evalState (runExceptT u) initFreshCounter of
          Left e -> e
          Right r -> error $ show r

testCheckLit :: [Test]
testCheckLit =
  [
    testCase "checkLit" $
    run (check emptyTEnv emptyUEnv (Lit (IntV 0)))
    @?= (M.fromList [],M.fromList [],TInt)

  , testCase "checkLit" $
    run (check emptyTEnv emptyUEnv (Lit (BoolV True)))
    @?= (M.fromList [],M.fromList [],TBool)

  , testCase "checkLit" $
    run (check emptyTEnv emptyUEnv (Lit (StringV "")))
    @?= (M.fromList [],M.fromList [],TString)
  ]

testCheckVar :: [Test]
testCheckVar =
  [
    testCase "checkVar" $
    run (check (M.fromList [("x",TInt)]) emptyUEnv (Var "x"))
    @?= (M.fromList [("x",TInt)],M.fromList [],TInt)

  , testCase "checkVar" $
    expectE (check emptyTEnv emptyUEnv (Var "x"))
    @?= UnboundVariable "x"
  ]

testCheckLet :: [Test]
testCheckLet =
  [
    testCase "checkLet: (let [x 0] x)" $
    run (check emptyTEnv emptyUEnv (Let (Var "x") (Lit (IntV 0)) (Var "x")))
    @?= (M.fromList [],M.fromList [],TInt)

  , testCase "checkLet: (let [x 0] true)" $
    run (check emptyTEnv emptyUEnv (Let (Var "x") (Lit (IntV 0)) (Lit (BoolV True))))
    @?= (M.fromList [],M.fromList [],TBool)

  , testCase "checkLet: {b -> Bool}: (let [x 0] b)" $
    run (check (M.fromList [("b",TBool)]) emptyUEnv (Let (Var "x") (Lit (IntV 0)) (Var "b")))
    @?= (M.fromList [("b",TBool)],M.fromList [],TBool)

  , testCase "checkLet: x" $
    expectE (check emptyTEnv emptyUEnv (Var "x"))
    @?= UnboundVariable "x"
  ]

testCheckNullFun :: [Test]
testCheckNullFun =
  [
    testCase "checkNullFun" $
    run (check emptyTEnv emptyUEnv (NullaryFun (Lit (IntV 0))))
    @?= (M.fromList [],M.fromList [],TInt)

  , testCase "checkNullFun" $
    expectE (check emptyTEnv emptyUEnv (NullaryFun (Var "x")))
    @?= UnboundVariable "x"
  ]

testcheckFun :: [Test]
testcheckFun =
  [
    testCase "checkFun: (fn [x] (fn [y] 0)) : (-> a (-> b Int))" $
    run (check emptyTEnv emptyUEnv (Fun (Var "x") (Fun (Var "y") (Lit (IntV 0)))))
    @?= (M.fromList [],M.fromList [],TFun (TVar "a") (TFun (TVar "b") TInt))

  -- TODO: Fix
  , testCase "checkFun: argument should not override outside scope: x : Bool => (fn [y] (fn [x] 0))" $
    run (check (M.fromList [("x",TBool)]) emptyUEnv (Fun (Var "y") (Fun (Var "x") (Lit (IntV 0)))))
    @?= (M.fromList [("x",TBool)],M.fromList [],TFun (TVar "a") (TFun (TVar "b") TInt))

  , testCase "checkFun: (fn [x] (fn [y] x)) : (-> a (-> b a))" $
    run (check emptyTEnv emptyUEnv (Fun (Var "x") (Fun (Var "y") (Var "x"))))
    @?= (M.fromList [],M.fromList [],TFun (TVar "a") (TFun (TVar "b") (TVar "a")))

  , testCase "checkFun: (fn [x] (x 3)) : (-> (-> Int a) a)" $
    run (check emptyTEnv emptyUEnv (Fun (Var "x") (UnaryApp (Var "x") (Lit (IntV 3)))))
    @?= (M.fromList [],M.fromList [("a",TFun TInt (TVar "b"))],TFun (TFun TInt (TVar "b")) (TVar "b"))

  , testCase "checkFun: (fn [x] (let [z (fn [y] (x 3))] x)) : (-> (-> Int a) (-> Int a))" $
    run (check emptyTEnv emptyUEnv (Fun (Var "x") (Let (Var "z") (Fun (Var "y") (UnaryApp (Var "x") (Lit (IntV 3)))) (Var "x"))))
    @?= (M.fromList [],M.fromList [("a",TFun TInt (TVar "c"))],TFun (TFun TInt (TVar "c")) (TFun TInt (TVar "c")))

  , testCase "checkFun: (fn [a] (fn [x] (a x))) : (-> (-> a b) (-> a b))" $
    run (check emptyTEnv emptyUEnv (Fun (Var "a") (Fun (Var "x") (UnaryApp (Var "a") (Var "x")))))
    @?= (M.fromList [],M.fromList [("a",TFun (TVar "b") (TVar "c"))],TFun (TFun (TVar "b") (TVar "c")) (TFun (TVar "b") (TVar "c")))

  -- TODO: Fix, what is this type? Can this be resolved with let-polymorphism?
  -- We would have to unify to the most general type.
  , testCase "checkFun: (fn [x] x x)" $
    expectE (check emptyTEnv emptyUEnv (Fun (Var "x") (UnaryApp (Var "x") (Var "x"))))
    @?= InfiniteType (TVar "a") (TFun (TVar "a") (TVar "b"))

  , testCase "checkFun: (fn [f] (fn [x] (f x))) : (-> (-> a b) (-> a b))" $
    (runCheck emptyTEnv emptyUEnv (Fun (Var "f") (Fun (Var "x") (UnaryApp (Var "f") (Var "x")))))
    @?= (M.fromList [],M.fromList [("a",TFun (TVar "b") (TVar "c"))],(TFun (TFun (TVar "b") (TVar "c")) (TFun (TVar "b") (TVar "c"))))

  -- tenv:
  -- f -> a
  -- x -> b
  --
  -- The "double" function from Pierce (pg 333)
  , testCase "checkFun: (fn [f] (fn [x] (f (f x)))) : (-> (-> a a) (-> a a))" $
    run (check emptyTEnv emptyUEnv (Fun (Var "f") (Fun (Var "x") (UnaryApp (Var "f") (UnaryApp (Var "f") (Var "x"))))))
    @?= (M.fromList [],M.fromList [("a",TFun (TVar "b") (TVar "c")),("c",TVar "b"),("d",TVar "b")],(TFun (TFun (TVar "b") (TVar "b")) (TFun (TVar "b") (TVar "b"))))

  -- Using the double function polymorphically.
  -- TODO write tests
  ]

testCheckNullApp :: [Test]
testCheckNullApp =
  [
    testCase "testNullApp" $
    run (check (M.fromList [("x",TBool)]) emptyUEnv (NullaryApp (Var "x")))
    @?= (M.fromList [("x",TBool)],M.fromList [],TBool)

  , testCase "testNullApp" $
    run (check emptyTEnv emptyUEnv (NullaryApp (NullaryFun (Lit (BoolV True)))))
    @?= (M.fromList [],M.fromList [],TBool)

  , testCase "testNullApp" $
    expectE (check emptyTEnv emptyUEnv (NullaryApp (Var "x")))
    @?= UnboundVariable "x"
  ]

testCheckUnaryApp :: [Test]
testCheckUnaryApp =
  [
    testCase "checkUnaryApp {x : (-> Int Bool)}: (x 0)" $
    run (check (M.fromList [("x",TFun TInt TBool)]) emptyUEnv (UnaryApp (Var "x") (Lit (IntV 0))))
    @?= (M.fromList [("x",TFun TInt TBool)],M.fromList [("a",TBool)],TBool)

  , testCase "checkUnaryApp: {f : (-> Bool Bool)}: (let [x true] (f x))" $
    run (check (M.fromList [("f",TFun TBool TBool)]) emptyUEnv (Let (Var "x") (Lit (BoolV True)) (UnaryApp (Var "f") (Var "x"))))
    @?= (M.fromList [("f",TFun TBool TBool)],M.fromList [("a",TBool)],TBool)

  , testCase "checkUnaryApp: {f : (-> Int Bool)}: ((fn [x] x) f)" $
    run (check (M.fromList [("f",TFun TInt TBool)]) emptyUEnv (UnaryApp (Fun (Var "x") (Var "x")) (Var "f")))
    @?= (M.fromList [("f",TFun TInt TBool)],M.fromList [("a",TFun TInt TBool),("b",TFun TInt TBool)],TFun TInt TBool)

  , testCase "checkUnaryApp: (let [x (fn [y] y)] (x 3))" $
    run (check emptyTEnv emptyUEnv (Let (Var "x") (Fun (Var "y") (Var "y")) (UnaryApp (Var "x") (Lit (IntV 3)))))
    @?= (M.fromList [],M.fromList [("a",TInt),("b",TInt)],TInt)

  , testCase "checkUnaryApp: (let [x (fn [y] y)] (x (x 3)))" $
    run (check emptyTEnv emptyUEnv (Let (Var "x") (Fun (Var "y") (Var "y")) (UnaryApp (Var "x") (UnaryApp (Var "x") (Lit (IntV 3))))))
    @?= (M.fromList [],M.fromList [("a",TInt),("b",TInt),("c",TInt)],TInt)

  , testCase "checkUnaryApp: ((fn [x] (fn [y] x)) 0)" $
    run (check emptyTEnv emptyUEnv (UnaryApp (Fun (Var "x") (Fun (Var "y") (Var "x"))) (Lit (IntV 0))))
    @?= (M.fromList [],M.fromList [("a",TInt),("c",TFun (TVar "b") TInt)],TFun (TVar "b") TInt)

  , testCase "checkUnaryApp: (((fn [x] (fn [y] x)) 0) True)" $
    run (check emptyTEnv emptyUEnv (UnaryApp (UnaryApp (Fun (Var "x") (Fun (Var "y") (Var "x"))) (Lit (IntV 0))) (Lit (BoolV True))))
    @?= (M.fromList [],M.fromList [("a",TInt),("b",TBool),("c",TFun (TVar "b") TInt),("d",TInt)],TInt)

  , testCase "checkUnaryApp: ((fn [x] x 3) (fn [x] x)) : Int" $
    run (check emptyTEnv emptyUEnv (UnaryApp (Fun (Var "x") (UnaryApp (Var "x") (Lit (IntV 3)))) (Fun (Var "x") (Var "x"))))
    @?= (M.fromList [],M.fromList [("a",TFun TInt (TVar "b")),("b",TInt),("c",TInt),("d",TInt)],TInt)

  , testCase "checkUnaryApp: (x 0) unbound variable" $
    expectE (check emptyTEnv emptyUEnv (UnaryApp (Var "x") (Lit (IntV 0))))
    @?= UnboundVariable "x"

  , testCase "checkUnaryApp: x : Int => (x 0) type mismatch" $
    expectE (check (M.fromList [("x",TInt)]) emptyUEnv (UnaryApp (Var "x") (Lit (IntV 0))))
    @?= FunctionExpected TInt

  -- TODO can this be type-checked with let-polymorphism?
  , testCase "checkUnaryApp: (let [x (fn [y] y)] (x x))" $
    expectE (check emptyTEnv emptyUEnv (Let (Var "x") (Fun (Var "y") (Var "y")) (UnaryApp (Var "x") (Var "x"))))
    @?= InfiniteType (TVar "a") (TFun (TVar "a") (TVar "a"))

  , testCase "checkUnaryApp: x : (-> Bool Int) => (x 0)" $
    expectE (check (M.fromList [("x",TFun TBool TInt)]) emptyUEnv (UnaryApp (Var "x") (Lit (IntV 0))))
    @?= Mismatch TBool TInt

  , testCase "checkUnaryApp: x : (-> z z) => ((fn [y] (y 3)) x) : Int" $
    run (check (M.fromList [("x", TFun (TVar "z") (TVar "z"))]) emptyUEnv (UnaryApp (Fun (Var "y") (UnaryApp (Var "y") (Lit (IntV 3)))) (Var "x")))
    @?= (M.fromList [("x",TFun (TVar "z") (TVar "z"))],M.fromList [("a",TFun TInt (TVar "b")),("b",TInt),("c",TInt),("z",TInt)],TInt)
  ]

testCheckList :: [Test]
testCheckList =
  [
    testCase "checkList: []" $
    run (check emptyTEnv emptyUEnv (List []))
    @?= (M.fromList [],M.fromList [],TList (TVar "a"))

  , testCase "checkList: [0]" $
    run (check emptyTEnv emptyUEnv (List [Lit (IntV 0)]))
    @?= (M.fromList [],M.fromList [],TList TInt)

  , testCase "checkList: [0 ((fn [x] x) 0) 123]" $
    run (check emptyTEnv emptyUEnv (List [Lit (IntV 0),UnaryApp (Fun (Var "x") (Var "x")) (Lit (IntV 0)), Lit (IntV 0)]))
    @?= (M.fromList [],M.fromList [("a",TInt),("b",TInt),("c",TInt),("d",TInt)],TList TInt)

  , testCase "checkList: [0 true]" $
    expectE (check emptyTEnv emptyUEnv (List [Lit (IntV 0),Lit (BoolV True)]))
    @?= Mismatch TInt TBool
  ]

testCheckIf :: [Test]
testCheckIf =
  [
    testCase "checkIf: (if ((fn [x] true) 0) \"truth\" \"false\") : String" $
    run (check emptyTEnv emptyUEnv (If (UnaryApp (Fun (Var "x") (Lit (BoolV True))) (Lit (IntV 0))) (Lit (StringV "then clause")) (Lit (StringV "else clause"))))
    @?= (M.fromList [],M.fromList [("a",TInt),("b",TBool)],TString)

  , testCase "checkIf: (if true (fn [x] x) (fn [y] y))" $
    run (check emptyTEnv emptyUEnv (If (Lit (BoolV False)) (Fun (Var "x") (Var "x")) (Fun (Var "y") (Var "y"))))
    @?= (M.fromList [],M.fromList [("b",TVar "a")],TFun (TVar "a") (TVar "a"))

  , testCase "checkIf: (if false 0 false)" $
    expectE (check emptyTEnv emptyUEnv (If (Lit (BoolV False)) (Lit (IntV 0)) (Lit (BoolV False))))
    @?= Mismatch TInt TBool
  ]
