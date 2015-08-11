module Neblen.Eval.Tests where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import qualified Data.Map.Strict as M

import Neblen.Eval
import Neblen.Data
import Neblen.Parser

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
    (eval' M.empty (p "(((fn [x] (fn [y] y)) 10) 20)"))
    @?= (Lit (IntV 20))

  , testCase "eval: ((fn [x y] (x y)) (fn [a] a) 3) => 3" $
    (eval' M.empty (p "((fn [x y] (x y)) (fn [a] a) 3)"))
    @?= (Lit (IntV 3))

  , testCase "eval: ((fn [x y z] (x y z)) (fn [a] a) (fn [b] b) 3) => 3" $
    (eval' M.empty (p "((fn [x y z] (x y z)) (fn [a] a) (fn [b] b) 3)"))
    @?= (Lit (IntV 3))
  ]

-- eval' M.empty (UnaryApp (UnaryApp (Fun (Var "x") (Fun (Var "y") (UnaryApp (Var "x") (Var "y")))) (Fun (Var "a") (Var "a"))) (Lit (IntV 3)))
-- eval' M.empty (UnaryApp (UnaryApp (UnaryApp (Fun (Var "x") (Fun (Var "y") (Fun (Var "z") (UnaryApp (UnaryApp (Var "x") (Var "y")) (Var "z"))))) (Fun (Var "a") (Var "a"))) (Fun (Var "b") (Var "b"))) (Lit (IntV 3)))

p :: NeblenProgram -> Exp
p np = case parseProgram np of
         Left _ -> error "wrong program"
         Right e -> e
