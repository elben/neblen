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

-- TODO unify funs with differing arity
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

run :: ExceptT TypeError (State FreshCounter) a -> a
run u = case evalState (runExceptT u) initFreshCounter of
          Left e -> error $ show e
          Right r -> r

expectE :: Show a => ExceptT TypeError (State FreshCounter) a -> TypeError
expectE u = case evalState (runExceptT u) initFreshCounter of
          Left e -> e
          Right r -> error $ "Expected error but got: " ++ show r

  -- Designing a new test style:
  --
  -- - Do we need to know the resulting Subst? Probably not; we should instead
  -- make our expressions TEST the Subst by the value returned (e.g. to see what
  -- the subst ended up as, return a value that takes advantage of that subst).
  --
  -- [("b","Bool"),
  --  ("c","Int")]  :=> "(let [x 0] b)"
  --                :=: "Bool"
  --
  -- "(let [x 0] b)" withEnv [("b","Bool"),
  --                          ("c","Int")]
  -- =~> "Bool"
  --
  -- "(let [x 0] true)" =~> "Bool"

withEnv ::
  String
  -> [(Name,String)]
  -> String
  -> Test
withEnv expr tenv t =
  testCase ("check: " ++ expr ++ " : " ++ t) $
  run (checkTypeWith (toExp expr) (M.fromList (map (second (toScheme . toType)) tenv)))
  @?= toType t

(~~>) :: a -> a
(~~>) = id

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

testCheckLit :: [Test]
testCheckLit =
  [
    "0" `withEnv` []
    ~~> "Int"

  , "true" =~> "Bool"
  , "\"\"" =~> "String"
  ]

testCheckVar :: [Test]
testCheckVar =
  [
    "x" `withEnv` [("x","Int")]
    ~~> "Int"

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
  , "(let [x 0] b)" `withEnv` [("b","Bool")] ~~> "Bool"

  -- let-polymorphism should look up existing type of 'f' when infering 'g'.
  , "((fn [f] (let [g f] (g 0))) (fn [x] true))" =~> "Bool"

  -- let-polymorphism on id function
  , "(let [id (fn [x] x) y (id 3) z (id true)] z)" =~> "Bool"
  , "(let [id (fn [x] x) y (id 3) z (id true)] y)" =~> "Int"
  , "(let [twice (fn [f x] (f (f x))) a (twice (fn [x] 10) 1) b (twice (fn [x] true) false)] a)" =~> "Int"
  , "(let [twice (fn [f x] (f (f x))) a (twice (fn [x] 10) 1) b (twice (fn [x] true) false)] b)" =~> "Bool"
  , "(let [twice (fn [f x] (f (f x))) a (twice (fn [x] 10) true)] a)" =!> Mismatch TInt TBool

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
  , "((fn [y] (fn [x] y)) x)" `withEnv` [("x","Bool")] ~~> "(-> a Bool)"

  -- The "double" function from Pierce (pg 333)
  , "(fn [f] (fn [x] (f (f x))))" =~> "(-> (-> a a) (-> a a))"

  , "(fn [x] (x x))" =!> InfiniteType (TVar "a") (TFun [TVar "a",TVar "b"])
  ]

testCheckNullApp :: [Test]
testCheckNullApp =
  [
    "(x)" `withEnv` [("x","(-> Bool)")] ~~> "Bool"
  , "((fn [] true))" =~> "Bool"
  , "(let [f (fn [] 0)] ((fn [f] (f)) f))" =~> "Int"
  , "(let [x true] (x))" =!> FunctionExpected TBool
  , "(x)" =!> UnboundVariable "x"
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
