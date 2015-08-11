module Neblen.Eval.Tests where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import qualified Data.Map.Strict as M

import Neblen.Eval
import Neblen.Data

tests :: Test
tests = testGroup "Neblen.Eval.Tests" $ concat
    [
      testLit
    , testUnaryApp
    ]

testLit :: [Test]
testLit =
  [
    testCase "eval: 1" $
    (eval' M.empty (Lit (IntV 1)))
    @?= (Lit (IntV 1))

  , testCase "eval: true" $
    (eval' M.empty (Lit (BoolV True)))
    @?= (Lit (BoolV True))

  , testCase "eval: \"hello\"" $
    (eval' M.empty (Lit (StringV "hello")))
    @?= (Lit (StringV "hello"))
  ]

testUnaryApp :: [Test]
testUnaryApp =
  [
    testCase "eval: (((fn [x] (fn [y] y)) 10) 20) => 20" $
    (eval' M.empty (UnaryApp (UnaryApp (Fun (Var "x") (Fun (Var "y") (Var "y"))) (Lit (IntV 10))) (Lit (IntV 20))))
    @?= (Lit (IntV 20))
  ]
