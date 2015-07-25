module Neblen.TypeChecker.Tests where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?), (@?=))
import Text.Printf (printf)
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import qualified Data.Map.Strict as M

import Neblen.Data
import Neblen.TypeChecker

tests :: Test
tests = testGroup "Neblen.TypeChecker.Tests" $ concat
    [
      testUnifier
    ]

-- uni :: UEnv -> Type -> Type -> (UEnv, Type) -> Test
-- uni uenv t1 t2 ans = testCase (show t1 ++ " <=> " ++ show t2) (run (unify uenv t1 t2) @?= ans)

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
    exceptE (unify (M.fromList [("a",TBool)]) (TVar "a") TInt)
    @=? (Mismatch TBool TInt)

  , testCase "unify: a <=> (-> a Int)" $
    exceptE (unify emptyUEnv (TVar "a") (TFun (TVar "a") TInt))
    @=? (InfiniteType (TVar "a") (TFun (TVar "a") TInt))
  ]

fromAssertions :: String       -- ^ Name
               -> [Assertion]  -- ^ Cases
               -> [Test]       -- ^ Result tests
fromAssertions name =
    zipWith testCase [printf "[%2d] %s" n name | n <- [1 :: Int ..]]

run :: UnifyCheck -> (UEnv, Type)
run u = case evalState (runExceptT u) initFreshCounter of
          Left e -> error $ show e
          Right r -> r

exceptE :: UnifyCheck -> TypeError
exceptE u = case evalState (runExceptT u) initFreshCounter of
          Left e -> e
          Right r -> error $ show r
