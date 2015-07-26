module Neblen.TypeChecker.Tests where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?), (@?=))
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import qualified Data.Map.Strict as M

import Neblen.Data
import Neblen.TypeChecker

tests :: Test
tests = testGroup "Neblen.TypeChecker.Tests" $ concat
    [
      testUnifier

    , testCheckLiteral
    , testCheckVar
    , testCheckLet
    , testCheckNullFun
    , testCheckFun
    , testCheckNullCall
    , testCheckUnaryCall
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
    @=? (M.fromList [("a",TInt),("b",TInt)],TFun TInt TInt)

  , testCase "unify: (-> Int a) <=> (-> a b)" $
    run (unify emptyUEnv (TFun TInt (TVar "a")) (TFun (TVar "a") (TVar "b")))
    @=? (M.fromList [("a",TInt),("b",TInt)],TFun TInt TInt)

  , testCase "unify: (-> Int a) <=> (-> b b)" $
    run (unify emptyUEnv (TFun TInt (TVar "a")) (TFun (TVar "b") (TVar "b")))
    @=? (M.fromList [("a",TInt),("b",TInt)],TFun TInt TInt)

  , testCase "unify: (-> a a) <=> (-> b b)" $
    run (unify (M.fromList [("b",TBool)]) (TFun (TVar "a") (TVar "a")) (TFun (TVar "a") (TVar "b")))
    @=? (M.fromList [("a",TBool),("b",TBool)],TFun TBool TBool)

  , testCase "unify: (-> a b) <=> (-> a b)" $
    run (unify (M.fromList [("a",TInt)]) (TFun (TVar "a") (TVar "b")) (TFun (TVar "a") (TVar "b")))
    @=? (M.fromList [("a",TInt)],TFun TInt (TVar "b"))

  , testCase "unify: (-> a b) <=> (-> c d)" $
    run (unify (M.fromList [("b",TBool)]) (TFun (TVar "a") (TVar "b")) (TFun (TVar "c") (TVar "d")))
    @=? (M.fromList [("a",TVar "c"),("b",TBool),("c",TVar "a"),("d",TBool)],TFun (TVar "c") TBool)

  , testCase "unify: (-> a a) <=> (-> a a)" $
    run (unify emptyUEnv (TFun (TVar "a") (TVar "a")) (TFun (TVar "a") (TVar "a")))
    @=? (M.fromList [],TFun (TVar "a") (TVar "a"))

  , testCase "unify: a <=> Int" $
    expectE (unify (M.fromList [("a",TBool)]) (TVar "a") TInt)
    @=? Mismatch TBool TInt

  , testCase "unify: a <=> (-> a Int)" $
    expectE (unify emptyUEnv (TVar "a") (TFun (TVar "a") TInt))
    @=? InfiniteType (TVar "a") (TFun (TVar "a") TInt)
  ]

run :: ExceptT TypeError (State FreshCounter) a -> a
run u = case evalState (runExceptT u) initFreshCounter of
          Left e -> error $ show e
          Right r -> r

expectE :: Show a => ExceptT TypeError (State FreshCounter) a -> TypeError
expectE u = case evalState (runExceptT u) initFreshCounter of
          Left e -> e
          Right r -> error $ show r

testCheckLiteral :: [Test]
testCheckLiteral =
  [
    testCase "checkLiteral" $
    run (checkExp emptyTEnv emptyUEnv (Literal (IntV 0)))
    @?= (M.fromList [],M.fromList [],TInt)

  , testCase "checkLiteral" $
    run (checkExp emptyTEnv emptyUEnv (Literal (BoolV True)))
    @?= (M.fromList [],M.fromList [],TBool)

  , testCase "checkLiteral" $
    run (checkExp emptyTEnv emptyUEnv (Literal (StringV "")))
    @?= (M.fromList [],M.fromList [],TString)
  ]

testCheckVar :: [Test]
testCheckVar =
  [
    testCase "checkVar" $
    run (checkExp (M.fromList [("x",TInt)]) emptyUEnv (Var "x"))
    @?= (M.fromList [("x",TInt)],M.fromList [],TInt)

  , testCase "checkVar" $
    expectE (checkExp emptyTEnv emptyUEnv (Var "x"))
    @?= UnboundVariable "x"
  ]

testCheckLet :: [Test]
testCheckLet =
  [
    testCase "checkLet" $
    run (checkExp emptyTEnv emptyUEnv (Let (Var "x") (Literal (IntV 0)) (Var "x")))
    @?= (M.fromList [],M.fromList [],TInt)

  , testCase "checkLet" $
    run (checkExp emptyTEnv emptyUEnv (Let (Var "x") (Literal (IntV 0)) (Literal (BoolV True))))
    @?= (M.fromList [],M.fromList [],TBool)

  , testCase "checkLet" $
    run (checkExp (M.fromList [("b",TBool)]) emptyUEnv (Let (Var "x") (Literal (IntV 0)) (Var "b")))
    @?= (M.fromList [("b",TBool)],M.fromList [],TBool)

  , testCase "checkLet" $
    expectE (checkExp emptyTEnv emptyUEnv (Var "x"))
    @?= UnboundVariable "x"
  ]

testCheckNullFun :: [Test]
testCheckNullFun =
  [
    testCase "checkNullFun" $
    run (checkExp emptyTEnv emptyUEnv (NullaryFun (Literal (IntV 0))))
    @?= (M.fromList [],M.fromList [],TInt)

  , testCase "checkNullFun" $
    expectE (checkExp emptyTEnv emptyUEnv (NullaryFun (Var "x")))
    @?= UnboundVariable "x"
  ]

testCheckFun :: [Test]
testCheckFun =
  [
    testCase "checkFun" $
    run (checkExp emptyTEnv emptyUEnv (Function (Var "x") (Function (Var "y") (Literal (IntV 0)))))
    @?= (M.fromList [],M.fromList [],TFun (TVar "x") (TFun (TVar "y") TInt))

  , testCase "checkFun: argument shuold not override outside scope\n\
            \ x : Bool\n\
            \ (fn [y] (fn [x] 0))" $
    run (checkExp (M.fromList [("x",TBool)]) emptyUEnv (Function (Var "y") (Function (Var "x") (Literal (IntV 0)))))
    @?= (M.fromList [("x",TBool)],M.fromList [],TFun (TVar "y") (TFun (TVar "x") TInt))

  , testCase "checkFun: (fn [x] (fn [y] x))" $
    run (checkExp emptyTEnv emptyUEnv (Function (Var "x") (Function (Var "y") (Var "x"))))
    @?= (M.fromList [],M.fromList [],TFun (TVar "x") (TFun (TVar "y") (TVar "x")))

  , testCase "checkfun: (fn [x] x 3)" $
    run (checkExp emptyTEnv emptyUEnv (Function (Var "x") (UnaryCall (Var "x") (Literal (IntV 3)))))
    @?= (M.fromList [],M.fromList [],TFun (TFun TInt (TVar "stillfree")) (TVar "stillfree"))

  , testCase "checkfun: (fn [x] x x)" $
    run (checkExp emptyTEnv emptyUEnv (Function (Var "x") (UnaryCall (Var "x") (Var "x"))))
    @?= (M.fromList [],M.fromList [],TFun (TVar "x") (TFun (TVar "x") (TVar "x")))
  ]

testCheckNullCall :: [Test]
testCheckNullCall =
  [
    testCase "testNullCall" $
    run (checkExp (M.fromList [("x",TBool)]) emptyUEnv (NullaryCall (Var "x")))
    @?= (M.fromList [("x",TBool)],M.fromList [],TBool)

  , testCase "testNullCall" $
    run (checkExp emptyTEnv emptyUEnv (NullaryCall (NullaryFun (Literal (BoolV True)))))
    @?= (M.fromList [],M.fromList [],TBool)

  , testCase "testNullCall" $
    expectE (checkExp emptyTEnv emptyUEnv (NullaryCall (Var "x")))
    @?= UnboundVariable "x"
  ]

testCheckUnaryCall :: [Test]
testCheckUnaryCall =
  [
    testCase "checkUnaryCall {x : (-> Int Bool)}: (x 0)" $
    run (checkExp (M.fromList [("x",TFun TInt TBool)]) emptyUEnv (UnaryCall (Var "x") (Literal (IntV 0))))
    @?= (M.fromList [("x",TFun TInt TBool)],M.fromList [],TBool)

  , testCase "checkUnaryCall:\n\
            \ f : (-> Bool Bool)\n\
            \ (let [x true] (f x))" $
    run (checkExp (M.fromList [("f",TFun TBool TBool)]) emptyUEnv (Let (Var "x") (Literal (BoolV True)) (UnaryCall (Var "f") (Var "x"))))
    @?= (M.fromList [("f",TFun TBool TBool)],M.fromList [],TBool)

  , testCase "checkUnaryCall:\n\
            \ f : (-> Int Bool)\n\
            \ ((fn [x] x) f)" $
    run (checkExp (M.fromList [("f",TFun TInt TBool)]) emptyUEnv (UnaryCall (Function (Var "x") (Var "x")) (Var "f")))
    @?= (M.fromList [("f",TFun TInt TBool)],M.fromList [("x",TFun TInt TBool)],TFun TInt TBool)

  , testCase "checkUnaryCall: (let [x (fn [y] y)] (x 3))" $
    run (checkExp emptyTEnv emptyUEnv (Let (Var "x") (Function (Var "y") (Var "y")) (UnaryCall (Var "x") (Literal (IntV 3)))))
    @?= (M.fromList [],M.fromList [("y",TInt)],TInt)

  , testCase "checkUnaryCall: (let [x (fn [y] y)] (x (x 3)))" $
    run (checkExp emptyTEnv emptyUEnv (Let (Var "x") (Function (Var "y") (Var "y")) (UnaryCall (Var "x") (UnaryCall (Var "x") (Literal (IntV 3))))))
    @?= (M.fromList [],M.fromList [("y",TInt)],TInt)

  , testCase "checkUnaryCall: ((fn [x] (fn [y] x)) 0)" $
    run (checkExp emptyTEnv emptyUEnv (UnaryCall (Function (Var "x") (Function (Var "y") (Var "x"))) (Literal (IntV 0))))
    @?= (M.fromList [],M.fromList [("x",TInt)],TFun (TVar "y") TInt)

  , testCase "checkUnaryCall: ((fn [x] (fn [y] x)) 0 True)" $
    run (checkExp emptyTEnv emptyUEnv (UnaryCall (UnaryCall (Function (Var "x") (Function (Var "y") (Var "x"))) (Literal (IntV 0))) (Literal (BoolV True))))
    @?= (M.fromList [],M.fromList [("x",TInt),("y",TBool)],TInt)

  , testCase "checkUnaryCall: ((fn [x] x 3) (fn [x] x))" $
    run (checkExp emptyTEnv emptyUEnv (UnaryCall (Function (Var "x") (UnaryCall (Var "x") (Literal (IntV 3)))) (Function (Var "x") (Var "x"))))
    @?= (M.fromList [],M.fromList [("stillfree",TInt),("x",TInt)],TInt)

  , testCase "checkUnaryCall" $
    expectE (checkExp emptyTEnv emptyUEnv (UnaryCall (Var "x") (Literal (IntV 0))))
    @?= UnboundVariable "x"

  , testCase "checkUnaryCall" $
    expectE (checkExp (M.fromList [("x",TInt)]) emptyUEnv (UnaryCall (Var "x") (Literal (IntV 0))))
    @?= UnboundVariable "x"

  , testCase "checkUnaryCall: (let [x (fn [y] y)] (x x))" $
    expectE (checkExp emptyTEnv emptyUEnv (Let (Var "x") (Function (Var "y") (Var "y")) (UnaryCall (Var "x") (Var "x"))))
    @?= UnboundVariable "x"
  ]

