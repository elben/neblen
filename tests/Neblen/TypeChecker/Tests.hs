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
    testCase "unify: Int <=> Int" $
    run (unify TInt TInt)
    @?= M.fromList []

  , testCase "unify: Int <=> a" $
    run (unify TInt (TVar "a"))
    @?= M.fromList [("a",TInt)]

  , testCase "unify: (-> Int Int) <=> (-> a b)" $
    run (unify (TFun [TInt,TInt]) (TFun [TVar "a",TVar "b"]))
    @?= M.fromList [("a",TInt),("b",TInt)]

  , testCase "unify: (-> Int a) <=> (-> a b)" $
    run (unify (TFun [TInt,TVar "a"]) (TFun [TVar "a",TVar "b"]))
    @?= M.fromList [("a",TInt),("b",TInt)]

  , testCase "unify: (-> Int a) <=> (-> b b)" $
    run (unify (TFun [TInt,TVar "a"]) (TFun [TVar "b",TVar "b"]))
    @?= M.fromList [("a",TInt),("b",TInt)]

  , testCase "unify: (-> a a) <=> (-> a a)" $
    run (unify (TFun [TVar "a",TVar "a"]) (TFun [TVar "a",TVar "a"]))
    @?= M.fromList []

  , testCase "unify: (-> a a) <=> (-> a b)" $
    run (unify (TFun [TVar "a",TVar "a"]) (TFun [TVar "a",TVar "b"]))
    @?= M.fromList [("a",TVar "b")]

  , testCase "unify: (-> a b) <=> (-> a b)" $
    run (unify (TFun [TVar "a",TVar "b"]) (TFun [TVar "a",TVar "b"]))
    @?= M.fromList []

  , testCase "unify: (-> a b) <=> (-> a c)" $
    run (unify (TFun [TVar "a",TVar "b"]) (TFun [TVar "a",TVar "c"]))
    @?= M.fromList [("b",TVar "c")]

  , testCase "unify: (-> a b) <=> (-> c b)" $
    run (unify (TFun [TVar "a",TVar "b"]) (TFun [TVar "c",TVar "b"]))
    @?= M.fromList [("a",TVar "c")]

  , testCase "unify: (-> a b) <=> (-> c d)" $
    run (unify (TFun [TVar "a",TVar "b"]) (TFun [TVar "c",TVar "d"]))
    @?= M.fromList [("a",TVar "c"),("b",TVar "d")]

  , testCase "unify: Bool <=> Int" $
    expectE (unify TBool TInt)
    @?= Mismatch TBool TInt

  , testCase "unify: a <=> (-> a Int)" $
    expectE (unify (TVar "a") (TFun [TVar "a",TInt]))
    @?= InfiniteType (TVar "a") (TFun [TVar "a",TInt])
  ]

run :: ExceptT TypeError (State FreshCounter) a -> a
run u = case evalState (runExceptT u) initFreshCounter of
          Left e -> error $ show e
          Right r -> r

expectE :: Show a => ExceptT TypeError (State FreshCounter) a -> TypeError
expectE u = case evalState (runExceptT u) initFreshCounter of
          Left e -> e
          Right r -> error $ "Expected error but got: " ++ show r

testCheckLit :: [Test]
testCheckLit =
  [
    testCase "checkLit: int" $
    run (check emptyTEnv emptySubst (Lit (IntV 0)))
    @?= (M.fromList [],TInt)

  , testCase "checkLit: bool" $
    run (check emptyTEnv emptySubst (Lit (BoolV True)))
    @?= (M.fromList [],TBool)

  , testCase "checkLit: string" $
    run (check emptyTEnv emptySubst (Lit (StringV "")))
    @?= (M.fromList [],TString)
  ]

testCheckVar :: [Test]
testCheckVar =
  [
    testCase "checkVar: {x -> Int}: x" $
    run (check (M.fromList [("x",toScheme TInt)]) emptySubst (Var "x"))
    @?= (M.fromList [],TInt)

  , testCase "checkVar: {}: x" $
    expectE (check emptyTEnv emptySubst (Var "x"))
    @?= UnboundVariable "x"
  ]

testCheckLet :: [Test]
testCheckLet =
  [
    testCase "checkLet: (let [x 0] x)" $
    run (check emptyTEnv emptySubst (Let (Var "x") (Lit (IntV 0)) (Var "x")))
    @?= (M.fromList [],TInt)

  , testCase "checkLet: (let [x 0] true)" $
    run (check emptyTEnv emptySubst (Let (Var "x") (Lit (IntV 0)) (Lit (BoolV True))))
    @?= (M.fromList [],TBool)

  , testCase "checkLet: {b -> Bool}: (let [x 0] b)" $
    run (check (M.fromList [("b",toScheme TBool)]) emptySubst (Let (Var "x") (Lit (IntV 0)) (Var "b")))
    @?= (M.fromList [],TBool)

    -- let-polymorphism should look up existing type of 'f' when infering 'g'.
  , testCase "checkLet let-polymorphism: ((fn [f] (let [g f] (g 0))) (fn [x] true))" $
    run (check emptyTEnv emptySubst (UnaryApp (Fun [Var "f"] (Let (Var "g") (Var "f") (UnaryApp (Var "g") (Lit (IntV 0))))) (Fun [Var "x"] (Lit (BoolV True)))))
    @?= (M.fromList [("a",TFun [TInt,TBool]),("b",TBool),("c",TInt),("d",TBool)],TBool)

  , testCase "checkLet id let-polymorphism: (let [id (fn [x] x), y (id 3), z (id true)] z)" $
    run (check emptyTEnv emptySubst (Let (Var "id") (Fun [Var "x"] (Var "x")) (Let (Var "y") (UnaryApp (Var "id") (Lit (IntV 3))) (Let (Var "z") (UnaryApp (Var "id") (Lit (BoolV True))) (Var "z")))))
    @?= (M.fromList [("b",TInt),("c",TInt),("d",TBool),("e",TBool)],TBool)

  , testCase "checkLet let-polymorphism: (let [twice (fn [f x] (f (f x))), a (twice (fn [x] 10) 1), b (twice (fn [x] true) false)] a)" $
    run (check emptyTEnv emptySubst (Let (Var "twice") (Fun [Var "f"] (Fun [Var "x"] (UnaryApp (Var "f") (UnaryApp (Var "f") (Var "x"))))) (Let (Var "a") (UnaryApp (UnaryApp (Var "twice") (Fun [Var "x"] (Lit (IntV 10)))) (Lit (IntV 1))) (Let (Var "b") (UnaryApp (UnaryApp (Var "twice") (Fun [Var "x"] (Lit (BoolV True)))) (Lit (BoolV True))) (Var "a")))))
    @?= (M.fromList [("a",TFun [TVar "d",TVar "d"]),("c",TVar "d"),("b",TVar "d"),("e",TInt),("f",TInt),("g",TFun [TInt,TInt]),("h",TInt),("i",TBool),("j",TBool),("k",TFun [TBool,TBool]),("l",TBool)],TInt)

  , testCase "checkLet let-polymorphism: (let [twice (fn [f x] (f (f x))), a (twice (fn [x] 10) true)] a)" $
    expectE (check emptyTEnv emptySubst (Let (Var "twice") (Fun [Var "f"] (Fun [Var "x"] (UnaryApp (Var "f") (UnaryApp (Var "f") (Var "x"))))) (Let (Var "a") (UnaryApp (UnaryApp (Var "twice") (Fun [Var "x"] (Lit (IntV 10)))) (Lit (IntV 1))) (Let (Var "b") (UnaryApp (UnaryApp (Var "twice") (Fun [Var "x"] (Lit (BoolV True)))) (Lit (IntV 10))) (Var "a")))))
    @?= Mismatch TBool TInt

    -- Checks that let-polymorphism doesn't generalize variables that are bound.
    -- Pierce pg 334.
  , testCase "checkLet let-polymorphism: ((fn [f] (let [g f] (g 0))) (fn [x] (if x x x)))" $
    expectE (check emptyTEnv emptySubst (UnaryApp (Fun [Var "f"] (Let (Var "g") (Var "f") (UnaryApp (Var "g") (Lit (IntV 0))))) (Fun [Var "x"] (If (Var "x") (Var "x") (Var "x")))))
    @?= Mismatch TBool (TVar "c")

  , testCase "checkLet: x" $
    expectE (check emptyTEnv emptySubst (Var "x"))
    @?= UnboundVariable "x"
  ]

testCheckNullFun :: [Test]
testCheckNullFun =
  [
    testCase "checkNullFun" $
    run (check emptyTEnv emptySubst (Fun [] (Lit (IntV 0))))
    @?= (M.fromList [],TFun [TInt])

  , testCase "checkNullFun" $
    expectE (check emptyTEnv emptySubst (Fun [] (Var "x")))
    @?= UnboundVariable "x"
  ]

testCheckFun :: [Test]
testCheckFun =
  [
    testCase "checkFun: (fn [x] (fn [y] 0)) : (-> a (-> b Int))" $
    run (check emptyTEnv emptySubst (Fun [Var "x"] (Fun [Var "y"] (Lit (IntV 0)))))
    @?= (M.fromList [],TFun [TVar "a",TFun [TVar "b",TInt]])

  , testCase "checkFun: argument should not override outside scope: x : Bool => (fn [y] (fn [x] 0))" $
    run (check (M.fromList [("x",toScheme TBool)]) emptySubst (Fun [Var "y"] (Fun [Var "x"] (Lit (IntV 0)))))
    @?= (M.fromList [],TFun [TVar "a",TFun [TVar "b",TInt]])

  , testCase "checkFun: (fn [x] (fn [y] x)) : (-> a (-> b a))" $
    run (check emptyTEnv emptySubst (Fun [Var "x"] (Fun [Var "y"] (Var "x"))))
    @?= (M.fromList [],TFun [TVar "a",TFun [TVar "b",TVar "a"]])

  , testCase "checkFun: (fn [x] (x 3)) : (-> (-> Int a) a)" $
    run (check emptyTEnv emptySubst (Fun [Var "x"] (UnaryApp (Var "x") (Lit (IntV 3)))))
    @?= (M.fromList [("a",TFun [TInt,TVar "b"])],TFun [TFun [TInt,TVar "b"],TVar "b"])

  , testCase "checkFun: (fn [x] (let [z (fn [y] (x 3))] x)) : (-> (-> Int a) (-> Int a))" $
    run (check emptyTEnv emptySubst (Fun [Var "x"] (Let (Var "z") (Fun [Var "y"] (UnaryApp (Var "x") (Lit (IntV 3)))) (Var "x"))))
    @?= (M.fromList [("a",TFun [TInt,TVar "c"])],TFun [TFun [TInt,TVar "c"],TFun [TInt,TVar "c"]])

  , testCase "checkFun: (fn [a] (fn [x] (a x))) : (-> (-> a b) (-> a b))" $
    run (check emptyTEnv emptySubst (Fun [Var "a"] (Fun [Var "x"] (UnaryApp (Var "a") (Var "x")))))
    @?= (M.fromList [("a",TFun [TVar "b",TVar "c"])],TFun [TFun [TVar "b",TVar "c"],TFun [TVar "b",TVar "c"]])

  , testCase "checkFun: (fn [f] (fn [x] (f x))) : (-> (-> a b) (-> a b))" $
    run (check emptyTEnv emptySubst (Fun [Var "f"] (Fun [Var "x"] (UnaryApp (Var "f") (Var "x")))))
    @?= (M.fromList [("a",TFun [TVar "b",TVar "c"])],TFun [TFun [TVar "b",TVar "c"],TFun [TVar "b",TVar "c"]])

  -- The "double" function from Pierce (pg 333)
  , testCase "checkFun: (fn [f] (fn [x] (f (f x)))) : (-> (-> a a) (-> a a))" $
    run (check emptyTEnv emptySubst (Fun [Var "f"] (Fun [Var "x"] (UnaryApp (Var "f") (UnaryApp (Var "f") (Var "x"))))))
    @?= (M.fromList [("a",TFun [TVar "d",TVar "d"]),("b",TVar "d"),("c",TVar "d")],TFun [TFun [TVar "d",TVar "d"],TFun [TVar "d",TVar "d"]])

  , testCase "checkFun: (fn [x] x x)" $
    expectE (check emptyTEnv emptySubst (Fun [Var "x"] (UnaryApp (Var "x") (Var "x"))))
    @?= InfiniteType (TVar "a") (TFun [TVar "a",TVar "b"])
  ]

testCheckNullApp :: [Test]
testCheckNullApp =
  [
    testCase "checkNullaryApp: {x => (-> Bool)} (x)" $
    run (check (M.fromList [("x",toScheme (TFun [TBool]))]) emptySubst (NullaryApp (Var "x")))
    @?= (M.fromList [("a",TBool)],TBool)

  , testCase "checkNullaryApp: ((fn [] true))" $
    run (check emptyTEnv emptySubst (NullaryApp (Fun [] (Lit (BoolV True)))))
    @?= (M.fromList [],TBool)

  , testCase "checkNullaryApp: {x => Bool} (x)" $
    expectE (check (M.fromList [("x",toScheme TBool)]) emptySubst (NullaryApp (Var "x")))
    @?= FunctionExpected TBool

  , testCase "checkNullaryApp: (x)" $
    expectE (check emptyTEnv emptySubst (NullaryApp (Var "x")))
    @?= UnboundVariable "x"
  ]

testCheckUnaryApp :: [Test]
testCheckUnaryApp =
  [
    testCase "checkUnaryApp {x : (-> Int Bool)}: (x 0)" $
    run (check (M.fromList [("x",toScheme $ TFun [TInt,TBool])]) emptySubst (UnaryApp (Var "x") (Lit (IntV 0))))
    @?= (M.fromList [("a",TBool)],TBool)

  , testCase "checkUnaryApp: {f : (-> Bool Bool)}: (let [x true] (f x))" $
    run (check (M.fromList [("f",toScheme $ TFun [TBool,TBool])]) emptySubst (Let (Var "x") (Lit (BoolV True)) (UnaryApp (Var "f") (Var "x"))))
    @?= (M.fromList [("a",TBool)],TBool)

  , testCase "checkUnaryApp: {f : (-> Int Bool)}: ((fn [x] x) f)" $
    run (check (M.fromList [("f",toScheme $ TFun [TInt,TBool])]) emptySubst (UnaryApp (Fun [Var "x"] (Var "x")) (Var "f")))
    @?= (M.fromList [("a",TFun [TInt,TBool]),("b",TFun [TInt,TBool])],TFun [TInt,TBool])

  , testCase "checkUnaryApp: (let [x (fn [y] y)] (x 3))" $
    run (check emptyTEnv emptySubst (Let (Var "x") (Fun [Var "y"] (Var "y")) (UnaryApp (Var "x") (Lit (IntV 3)))))
    @?= (M.fromList [("b",TInt),("c",TInt)],TInt)

  , testCase "checkUnaryApp: (let [x (fn [y] y)] (x (x 3)))" $
    run (check emptyTEnv emptySubst (Let (Var "x") (Fun [Var "y"] (Var "y")) (UnaryApp (Var "x") (UnaryApp (Var "x") (Lit (IntV 3))))))
    @?= (M.fromList [("b",TInt),("c",TInt),("d",TInt),("e",TInt)],TInt)

  , testCase "checkUnaryApp: ((fn [x] (fn [y] x)) 0)" $
    run (check emptyTEnv emptySubst (UnaryApp (Fun [Var "x"] (Fun [Var "y"] (Var "x"))) (Lit (IntV 0))))
    @?= (M.fromList [("a",TInt),("c",TFun [TVar "b",TInt])],TFun [TVar "b",TInt])

  , testCase "checkUnaryApp: (((fn [x] (fn [y] x)) 0) True)" $
    run (check emptyTEnv emptySubst (UnaryApp (UnaryApp (Fun [Var "x"] (Fun [Var "y"] (Var "x"))) (Lit (IntV 0))) (Lit (BoolV True))))
    @?= (M.fromList [("a",TInt),("b",TBool),("c",TFun [TBool,TInt]),("d",TInt)],TInt)

  , testCase "checkUnaryApp: ((fn [x] x 3) (fn [x] x)) : Int" $
    run (check emptyTEnv emptySubst (UnaryApp (Fun [Var "x"] (UnaryApp (Var "x") (Lit (IntV 3)))) (Fun [Var "x"] (Var "x"))))
    @?= (M.fromList [("a",TFun [TInt,TInt]),("b",TInt),("c",TInt),("d",TInt)],TInt)

  , testCase "checkUnaryApp: (let [x (fn [y] y)] (x x))" $
    run (check emptyTEnv emptySubst (Let (Var "x") (Fun [Var "y"] (Var "y")) (UnaryApp (Var "x") (Var "x"))))
    @?= (M.fromList [("b",TFun [TVar "c",TVar "c"]),("d",TFun [TVar "c",TVar "c"])],TFun [TVar "c",TVar "c"])

  , testCase "checkUnaryApp: (x 0) unbound variable" $
    expectE (check emptyTEnv emptySubst (UnaryApp (Var "x") (Lit (IntV 0))))
    @?= UnboundVariable "x"

  , testCase "checkUnaryApp: x : Int => (x 0) type mismatch" $
    expectE (check (M.fromList [("x",toScheme TInt)]) emptySubst (UnaryApp (Var "x") (Lit (IntV 0))))
    @?= FunctionExpected TInt

  , testCase "checkUnaryApp: x : (-> Bool Int) => (x 0)" $
    expectE (check (M.fromList [("x",toScheme $ TFun [TBool,TInt])]) emptySubst (UnaryApp (Var "x") (Lit (IntV 0))))
    @?= Mismatch TBool TInt

  , testCase "checkUnaryApp: x : (-> z z) => ((fn [y] (y 3)) x) : Int" $
    run (check (M.fromList [("x",toScheme $ TFun [TVar "z",TVar "z"])]) emptySubst (UnaryApp (Fun [Var "y"] (UnaryApp (Var "y") (Lit (IntV 3)))) (Var "x")))
    @?= (M.fromList [("a",TFun [TInt,TInt]),("b",TInt),("c",TInt),("z",TInt)],TInt)
  ]

testCheckList :: [Test]
testCheckList =
  [
    testCase "checkList: []" $
    run (check emptyTEnv emptySubst (List []))
    @?= (M.fromList [],TList (TVar "a"))

  , testCase "checkList: [0]" $
    run (check emptyTEnv emptySubst (List [Lit (IntV 0)]))
    @?= (M.fromList [],TList TInt)

  , testCase "checkList: [0 ((fn [x] x) 0) 123]" $
    run (check emptyTEnv emptySubst (List [Lit (IntV 0),UnaryApp (Fun [Var "x"] (Var "x")) (Lit (IntV 0)), Lit (IntV 0)]))
    @?= (M.fromList [("a",TInt),("b",TInt),("c",TInt),("d",TInt)],TList TInt)

  , testCase "checkList: [0 true]" $
    expectE (check emptyTEnv emptySubst (List [Lit (IntV 0),Lit (BoolV True)]))
    @?= Mismatch TInt TBool
  ]

testCheckIf :: [Test]
testCheckIf =
  [
    testCase "checkIf: (if ((fn [x] true) 0) \"truth\" \"false\") : String" $
    run (check emptyTEnv emptySubst (If (UnaryApp (Fun [Var "x"] (Lit (BoolV True))) (Lit (IntV 0))) (Lit (StringV "then clause")) (Lit (StringV "else clause"))))
    @?= (M.fromList [("a",TInt),("b",TBool)],TString)

  , testCase "checkIf: (if true (fn [x] x) (fn [y] y))" $
    run (check emptyTEnv emptySubst (If (Lit (BoolV False)) (Fun [Var "x"] (Var "x")) (Fun [Var "y"] (Var "y"))))
    @?= (M.fromList [("a",TVar "b")],TFun [TVar "b",TVar "b"])

  , testCase "checkIf: (if false 0 false)" $
    expectE (check emptyTEnv emptySubst (If (Lit (BoolV False)) (Lit (IntV 0)) (Lit (BoolV False))))
    @?= Mismatch TInt TBool
  ]
