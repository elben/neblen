module Neblen.Parser where

import Neblen.Data
import Text.ParserCombinators.Parsec
import qualified Control.Applicative as A
import qualified Data.Set as S
import Control.Monad

-- $setup
-- >>> import Data.Either

-- | Skip one or more spaces.
skipSpaces1 :: Parser ()
skipSpaces1 = skipMany1 space

-- | Parse string.
--
-- >>> parse parseString "" "\"Hello\""
-- Right (Lit (StringV "Hello"))
--
-- >>> parse parseString "" "\"\""
-- Right (Lit (StringV ""))
--
-- >>> isLeft $ parse parseString "" "Hello"
-- True
--
-- >>> isLeft $ parse parseString "" "\"Hello"
-- True
--
parseString :: Parser Exp
parseString = do
  s <- between (char '"') (char '"') (many (noneOf "\""))
  return $ Lit (StringV s)

-- | Parse Boolean.
--
-- >>> parse parseBool "" "true"
-- Right (Lit (BoolV True))
--
-- >>> parse parseBool "" "false"
-- Right (Lit (BoolV False))
--
-- >>> isLeft $ parse parseBool "" "\"true\""
-- True
--
parseBool :: Parser Exp
parseBool = do
  -- 'string' consumes input, so use 'try' so that no input is consumed if the
  -- literal is not true or false. This way, variables that are prefixed with
  -- any prefix of "true" or "false" can be parsed (e.g. 'fx' or 'true-thing').
  s <- try (string "true") <|> try (string "false")
  return $ Lit (BoolV (s == "true"))

-- | Parse Integers.
--
-- >>> parse parseInt "" "0"
-- Right (Lit (IntV 0))
--
-- >>> parse parseInt "" "123"
-- Right (Lit (IntV 123))
--
-- >>> parse parseInt "" "123.456"
-- Right (Lit (IntV 123))
--
-- >>> isLeft $ parse parseInt "" ".456"
-- True
--
parseInt :: Parser Exp
parseInt = do
  s <- many1 digit
  return $ Lit (IntV (read s :: Int))

-- | Parse variable identifier. Identifiers must be prefixed with either a
-- letter, dash (-) or underscore (_). Or a reserved symbol identifier.
--
-- >>> parse parseVar "" "x"
-- Right (Var "x")
--
-- >>> parse parseVar "" "abc-def"
-- Right (Var "abc-def")
--
-- >>> parse parseVar "" ">>="
-- Right (Var ">>=")
--
-- >>> parse parseVar "" "-"
-- Right (Var "-")
--
-- >>> isLeft $ parse parseVar "" "_"
-- True
--
-- >>> isLeft $ parse parseVar "" "fn"
-- True
--
parseVar :: Parser Exp
parseVar = do
  i <- try parseAlphaNumericId <|> parseSymbolId
  if S.member i reservedIds
    then unexpected $ "reserved identifier '" ++ i ++ "'"
    else return $ Var i

-- | Parse an alpha-numeric identifier.
--
-- >>> parse parseAlphaNumericId "" "abc"
-- Right "abc"
--
-- >>> parse parseAlphaNumericId "" "_abc"
-- Right "_abc"
--
-- >>> parse parseAlphaNumericId "" "-abc_def-123-"
-- Right "-abc_def-123-"
--
-- >>> isLeft $ parse parseAlphaNumericId "" "-"
-- True
--
-- >>> isLeft $ parse parseAlphaNumericId "" "-123"
-- True
--
-- >>> isLeft $ parse parseAlphaNumericId "" "_"
-- True
--
-- >>> isLeft $ parse parseAlphaNumericId "" "123abc"
-- True
--
parseAlphaNumericId :: Parser String
parseAlphaNumericId = do
  -- Get first character.
  p <- letter <|> char '-' <|> char '_'

  -- If first character was a - or _, then needs to be followed one letter, then
  -- zero or more alphanumerics (e.g. -123 is invalid, but -abc is valid).
  --
  -- Else, it can be followed by zero or more alphanumerics.
  --
  -- TODO: How to simplify this?
  if p == '-' || p == '_'
    then (do
      p' <- letter
      rest <- restOfAlphaNumericId
      return (p : p' : rest))
    else (do
      rest <- restOfAlphaNumericId
      return (p : rest))

restOfAlphaNumericId :: Parser String
restOfAlphaNumericId = many (alphaNum <|> char '-' <|> char '_') <?> "Non-symbolic identifier must consist of alphanumeric characters, dashes and underscores."

-- | Parse a symbol-only identifier.
--
-- >>> parse parseSymbolId "" ">= abc"
-- Right ">="
--
-- >>> parse parseSymbolId "" "+"
-- Right "+"
--
-- >>> parse parseSymbolId "" "+++++"
-- Right "+++++"
--
-- Bind operator.
-- >>> parse parseSymbolId "" ">>="
-- Right ">>="
--
-- >>> parse parseSymbolId "" "+3"
-- Right "+"
--
-- >>> isLeft $ parse parseSymbolId "" "!!!"
-- True
--
parseSymbolId :: Parser String
parseSymbolId = many1 symbolIds -- ID must be one or more symbols

-- | Symbols that can be part of symbol-only identifiers.
symbolIds :: Parser Char
symbolIds = oneOf validIdSymbols

-- | Parse lists. S-expressions!
--
-- >>> parse parseList "" "()"
-- Right (List [])
--
-- >>> parse parseList "" "(list xyz-abc \"abc\" 123)"
-- Right (List [Var "xyz-abc",Lit (StringV "abc"),Lit (IntV 123)])
--
-- >>> parse parseList "" "(list xyz-abc (list 0 \"foo\" true))"
-- Right (List [Var "xyz-abc",List [Lit (IntV 0),Lit (StringV "foo"),Lit (BoolV True)]])
--
-- >>> parse parseList "" "(list (list 0 \"foo\" true))"
-- Right (List [List [Lit (IntV 0),Lit (StringV "foo"),Lit (BoolV True)]])
--
-- >>> isLeft $ parse parseList "" "(def x 123)"
-- True
--
-- >>> isLeft $ parse parseList "" "(x 123 y)"
-- True
--
-- >>> isLeft $ parse parseList "" "(fn [x] (+ x 123))"
-- True
--
-- >>> isLeft $ parse parseList "" "(123 456"
-- True
--
parseList :: Parser Exp
parseList = parseEmptyList <|> parseList'

-- | Parse empty list '()'.
--
-- >>> parse parseEmptyList "" "()"
-- Right (List [])
--
-- >>> isLeft $ parse parseEmptyList "" "(123)"
-- True
--
parseEmptyList :: Parser Exp
parseEmptyList = try (string "()") A.*> A.pure (List [])

-- | Parse list with 'list' keyword.
--
-- >>> parse parseList' "" "(list)"
-- Right (List [])
--
-- >>> parse parseList' "" "(list   1 abc-xyz \"abc\")"
-- Right (List [Lit (IntV 1),Var "abc-xyz",Lit (StringV "abc")])
--
parseList' :: Parser Exp
parseList' = try (string "(list)") A.*> A.pure (List []) <|> try (parseListWithSurroundingPrefix (Just (string "list")) '(' ')' parseExps List)

-- | Parse vectors (which are just lists with different syntax, for now).
--
-- >>> parse parseVector "" "[]"
-- Right (List [])
--
-- >>> parse parseVector "" "[xyz-abc [0 \"foo\" true]]"
-- Right (List [Var "xyz-abc",List [Lit (IntV 0),Lit (StringV "foo"),Lit (BoolV True)]])
--
parseVector :: Parser Exp
parseVector = parseListWithSurrounding '[' ']' parseExps List

-- | Parse many @p@s, separated by at least one space.
parseMany :: Parser a -> Parser [a]
parseMany p = sepBy p skipSpaces1

parseExps :: Parser [Exp]
parseExps = parseMany parseExp

parseListWithSurroundingPrefix ::
  Maybe (Parser String)
  -- ^ Optional prefix parser
  -> Char -> Char
  -- ^ Start and begin char
  -> Parser [a]
  -- Parse multiple of these things
  -> ([a] -> a)
  -- Convert multiple things into one
  -> Parser a
parseListWithSurroundingPrefix mp l r ps f = do
  _ <- char l
  case mp of
    Just s -> s A.*> skipSpaces1
    _      -> spaces

  -- Must be separated by at least one space
  exps <- ps

  _ <- char r
  return $ f exps

parseListWithSurrounding :: Char -> Char -> Parser [a] -> ([a] -> a) -> Parser a
parseListWithSurrounding = parseListWithSurroundingPrefix Nothing

-- | Parse definition.
--
-- >>> parse parseDef "" "(def x 123)"
-- Right (Def (Var "x") (Lit (IntV 123)))
--
parseDef :: Parser Exp
parseDef = try $ do
  _ <- char '('
  _ <- try $ string "def"
  skipSpaces1
  var <- parseVar
  skipSpaces1
  body <- parseExp
  _ <- char ')'
  return $ Def var body

-- | Parse unary function calls.
--
-- >>> parse parseUnaryApp "" "(x 123)"
-- Right (UnaryApp (Var "x") (Lit (IntV 123)))
--
-- Curry (x 1 2) as ((x 1) 2):
--
-- >>> parse parseUnaryApp "" "(x 1 2)"
-- Right (UnaryApp (UnaryApp (Var "x") (Lit (IntV 1))) (Lit (IntV 2)))
--
-- >>> parse parseUnaryApp "" "(x 1 2 3)"
-- Right (UnaryApp (UnaryApp (UnaryApp (Var "x") (Lit (IntV 1))) (Lit (IntV 2))) (Lit (IntV 3)))
--
-- >>> parse parseUnaryApp "" "((fn [x y z] (+ x y)) 1 2 3)"
-- Right (UnaryApp (UnaryApp (UnaryApp (Fun [Var "x",Var "y",Var "z"] (UnaryApp (UnaryApp (Var "+") (Var "x")) (Var "y"))) (Lit (IntV 1))) (Lit (IntV 2))) (Lit (IntV 3)))
--
parseUnaryApp :: Parser Exp
parseUnaryApp = try $ do
  _ <- char '('
  varOrFn <- parseExp
  skipSpaces1
  args <- parseExps
  _ <- char ')'
  return $ buildAppStack varOrFn args

-- | Convert a function call with multiple arguments to recursive unary calls.
-- If no arguments are given, return a nullary function call.
--
-- >>> buildAppStack (Var "x") []
-- NullaryApp (Var "x")
--
-- >>> buildAppStack (Var "x") [Lit (IntV 1)]
-- UnaryApp (Var "x") (Lit (IntV 1))
--
-- >>> buildAppStack (Var "x") [Lit (IntV 1),Lit (IntV 2),Lit (IntV 3)]
-- UnaryApp (UnaryApp (UnaryApp (Var "x") (Lit (IntV 1))) (Lit (IntV 2))) (Lit (IntV 3))
--
buildAppStack :: Exp -> [Exp] -> Exp
buildAppStack fn [] = NullaryApp fn
buildAppStack fn [arg] = UnaryApp fn arg
buildAppStack fn (a:as) = buildAppStack (UnaryApp fn a) as

-- | Parse nullary function calls. That is, functions with no arguments.
--
-- >>> parse parseNullaryApp "" "(+)"
-- Right (NullaryApp (Var "+"))
--
-- >>> parse parseNullaryApp "" "((fn [] 0))"
-- Right (NullaryApp (Fun [] (Lit (IntV 0))))
--
-- >>> parse parseNullaryApp "" "(list)"
-- Right (NullaryApp (Var "list"))
--
-- >>> isLeft $ parse parseNullaryApp "" "()"
-- True
--
parseNullaryApp :: Parser Exp
parseNullaryApp = do
  _ <- char '('
  varOrFn <- parseExp
  _ <- char ')'
  return $ NullaryApp varOrFn

-- | Parse functions.
--
-- >>> parse parseFun "" "(fn [x y z] (x y z))"
-- Right (Fun [Var "x",Var "y",Var "z"] (UnaryApp (UnaryApp (Var "x") (Var "y")) (Var "z")))
--
-- >>> parse parseFun "" "(fn [] 3)"
-- Right (Fun [] (Lit (IntV 3)))
--
parseFun :: Parser Exp
parseFun = do
  parseStartsListWith "fn"
  argsVec <- parseVecOfVars
  body <- parseBodyOfFun
  return $ Fun argsVec body

parseStartsListWith :: String -> Parser ()
parseStartsListWith keyword = do
  _ <- char '('
  _ <- try $ string keyword
  skipSpaces1

parseBodyOfFun :: Parser Exp
parseBodyOfFun = do
  _ <- skipSpaces1
  body <- parseExp
  _ <- char ')'
  return body

-- | Parse vector of vars.
--
-- >>> parse parseVecOfVars "" "[]"
-- Right []
--
-- >>> parse parseVecOfVars "" "[x y z]"
-- Right [Var "x",Var "y",Var "z"]
--
parseVecOfVars :: Parser [Exp]
parseVecOfVars = do
  _ <- char '['
  vars <- sepBy parseVar skipSpaces1
  _ <- char ']'
  return vars

-- | Parse let expressions.
--
-- >>> parse parseLet "" "(let [x 1 y 2] (+ x y))"
-- Right (Let (Var "x") (Lit (IntV 1)) (Let (Var "y") (Lit (IntV 2)) (UnaryApp (UnaryApp (Var "+") (Var "x")) (Var "y"))))
--
-- >>> isLeft $ parse parseLet "" "(let [] (+ x y))"
-- True
--
-- >>> isLeft $ parse parseLet "" "(let [x 1 y] (+ x y))"
-- True
--
parseLet :: Parser Exp
parseLet = do
  parseStartsListWith "let"
  bindings <- parseVarExpPairs
  body <- parseBodyOfFun
  return $ buildLetBindingStack body bindings

buildLetBindingStack :: Exp -> [(Exp, Exp)] -> Exp
buildLetBindingStack _ [] = error "let must bind variables"
buildLetBindingStack body [(Var v,bind)] = Let (Var v) bind body
buildLetBindingStack body ((Var v,bind):bindings) = Let (Var v) bind (buildLetBindingStack body bindings)
buildLetBindingStack _ _ = error "let has invalid bindings"

-- | Parse (var, exp) pairs. Must have at least one.
--
-- >>> parse parseVarExpPairs "" "[x 1 y 2]"
-- Right [(Var "x",Lit (IntV 1)),(Var "y",Lit (IntV 2))]
--
-- >>> isLeft $ parse parseLet "" "[]"
-- True
--
-- >>> isLeft $ parse parseLet "" "[x 1 y]"
-- True
--
parseVarExpPairs :: Parser [(Exp, Exp)]
parseVarExpPairs = do
  _ <- char '['
  pairs <- sepBy1 parseVarExpPair skipSpaces1
  _ <- char ']'
  return pairs

parseVarExpPair :: Parser (Exp, Exp)
parseVarExpPair = do
  var <- parseVar
  _ <- skipSpaces1
  body <- parseExp
  return (var, body)

-- | Parse if.
--
-- >>> parse parseIf "" "(if true (x 1) (y 1))"
-- Right (If (Lit (BoolV True)) (UnaryApp (Var "x") (Lit (IntV 1))) (UnaryApp (Var "y") (Lit (IntV 1))))
--
parseIf :: Parser Exp
parseIf = do
  parseStartsListWith "if"
  p <- parseExp
  _ <- skipSpaces1
  t <- parseExp
  e <- parseBodyOfFun
  return (If p t e)

-- | Parse upper-cased string.
--
-- >>> parse parseUpperCasedString "" "F"
-- Right "F"
--
-- >>> parse parseUpperCasedString "" "Foo"
-- Right "Foo"
--
-- >>> parse parseUpperCasedString "" "FooBarFar"
-- Right "FooBarFar"
--
-- >>> parse parseUpperCasedString "" "FooBar Far"
-- Right "FooBar"
--
-- >>> isLeft $ parse parseUpperCasedString "" "fooBarFar"
-- True
--
parseUpperCasedString :: Parser String
parseUpperCasedString = do
  u <- upper
  s <- many letter
  return $ u : s

parseDataTypeWithoutTvars :: Parser (Name,[TName])
parseDataTypeWithoutTvars = do
  name <- parseUpperCasedString
  return (name, [])

parseDataTypeWithTvars :: Parser (Name,[TName])
parseDataTypeWithTvars = do
  _ <- char '('
  name <- parseUpperCasedString
  tvars <- many (spaces >> many1 lower)
  _ <- char ')'
  return (name, tvars)


-- | Parse data type name.
--
-- >>> parse parseDataTypeName "" "Animal"
-- Right ("Animal",[])
--
-- >>> parse parseDataTypeName "" "(Foo a b c)"
-- Right ("Foo",["a","b","c"])
--
parseDataTypeName :: Parser (Name,[TName])
parseDataTypeName = parseDataTypeWithoutTvars <|> parseDataTypeWithTvars

-- parseDatatTypeWithTapps :: Parser DeclareCtor

-- | Parse data type constructor.
--
-- >>> parse parseDataTypeConstructor "" "Nothing"
-- Right (DeclareCtor "Nothing" [])
--
-- >>> parse parseDataTypeConstructor "" "(Nothing)"
-- Right (DeclareCtor "Nothing" [])
--
-- >>> parse parseDataTypeConstructor "" "(Just a)"
-- Right (DeclareCtor "Just" [a : k?])
--
-- >>> parse parseDataTypeConstructor "" "(Just a b c)"
-- Right (DeclareCtor "Just" [a : k?,b : k?,c : k?])
--
-- >>> parse parseDataTypeConstructor "" "(Branch (Tree a) (Tree a))"
-- Right (DeclareCtor "Branch" [(Tree a : k?),(Tree a : k?)])
--
-- >>> parse parseDataTypeConstructor "" "(Branch (Tree a) (Foo a b c))"
-- Right (DeclareCtor "Branch" [(Tree a : k?),(((Foo a : k?) b : k?) c : k?)])
--
-- Below, Foo is another abstract data type. This is allowed:
--
--  Foo = Bar | Far
--  Tree a = Leaf a | Branch (Tree a) Foo -- refers to the Foo above
--
-- >>> parse parseDataTypeConstructor "" "(Branch (Tree a) Foo)"
-- Right (DeclareCtor "Branch" [(Tree a : k?),Foo])
--
-- >>> isLeft $ parse parseDataTypeConstructor "" "(abc)"
-- True
--
-- >>> isLeft $ parse parseDataTypeConstructor "" "abc"
-- True
--
parseDataTypeConstructor :: Parser DeclareCtor
parseDataTypeConstructor = parseDataTypeConstructorSingleton <|> parseDataTypeConstructorWithParens

-- | Parse data type constructors.
--
-- >>> parse parseDataTypeConstructorWithParens "" "(Branch (Tree a) (Foo a b c))"
-- Right (DeclareCtor "Branch" [(Tree a : k?),(((Foo a : k?) b : k?) c : k?)])
--
parseDataTypeConstructorWithParens :: Parser DeclareCtor
parseDataTypeConstructorWithParens = do
  _ <- char '('
  name <- parseUpperCasedString
  _ <- spaces
  types <- parseMany parseTypeKind
  _ <- char ')'
  return (DeclareCtor name types)

parseDataTypeConstructorSingleton :: Parser DeclareCtor
parseDataTypeConstructorSingleton = do
  name <- parseUpperCasedString
  return (DeclareCtor name [])

-- | Parse data type.
--
-- >>> parse parseDataType "" "(data-type Animal Dog Cat Cow)"
-- Right (DeclareType "Animal" [] [DeclareCtor "Dog" [],DeclareCtor "Cat" [],DeclareCtor "Cow" []] k?)
--
-- >>> parse parseDataType "" "(data-type (Maybe a) (Just a) Nothing)"
-- Right (DeclareType "Maybe" ["a"] [DeclareCtor "Just" [a : k?],DeclareCtor "Nothing" []] k?)
--
-- Invalid data type, but it is parsed. Should error in type check.
-- >>> parse parseDataType "" "(data-type (Maybe a) (Just a b c) Nothing)"
-- Right (DeclareType "Maybe" ["a"] [DeclareCtor "Just" [a : k?,b : k?,c : k?],DeclareCtor "Nothing" []] k?)
--
-- >>> parse parseDataType "" "(data-type (Tree a) (Leaf a) (Branch (Tree a) (Tree a)))"
-- Right (DeclareType "Tree" ["a"] [DeclareCtor "Leaf" [a : k?],DeclareCtor "Branch" [(Tree a : k?),(Tree a : k?)]] k?)
--
-- >>> parse parseDataType "" "(data-type (Tree a) (Leaf a) (Branch (Tree a) Foo))"
-- Right (DeclareType "Tree" ["a"] [DeclareCtor "Leaf" [a : k?],DeclareCtor "Branch" [(Tree a : k?),Foo]] k?)
--
-- >>> parse parseDataType "" "(data-type (Either a b) (Left a) (Right b))"
-- Right (DeclareType "Either" ["a","b"] [DeclareCtor "Left" [a : k?],DeclareCtor "Right" [b : k?]] k?)
--
parseDataType :: Parser DeclareType
parseDataType = do
  parseStartsListWith "data-type"
  (name, tvars) <- parseDataTypeName
  _ <- spaces
  ctors <- parseMany parseDataTypeConstructor
  _ <- char ')'
  return (DeclareType name tvars ctors KUnknownInit)

-- | Parse expression.
--
-- >>> parse parseExp "" "\"abc\""
-- Right (Lit (StringV "abc"))
--
-- >>> parse parseExp "" "true"
-- Right (Lit (BoolV True))
--
-- >>> parse parseExp "" "123"
-- Right (Lit (IntV 123))
--
-- >>> parse parseExp "" "x"
-- Right (Var "x")
--
-- >>> parse parseExp "" "[+ -]"
-- Right (List [Var "+",Var "-"])
--
-- >>> parse parseExp "" "(list + - abc)"
-- Right (List [Var "+",Var "-",Var "abc"])
--
-- >>> parse parseExp "" "+ 13"
-- Right (Var "+")
--
-- >>> parse parseExp "" "(def x 123)"
-- Right (Def (Var "x") (Lit (IntV 123)))
--
-- >>> parse parseExp "" "(foo bar)"
-- Right (UnaryApp (Var "foo") (Var "bar"))
--
-- >>> parse parseExp "" "(foo)"
-- Right (NullaryApp (Var "foo"))
--
-- >>> parse parseExp "" "(let [x 1 y 2] (+ x y))"
-- Right (Let (Var "x") (Lit (IntV 1)) (Let (Var "y") (Lit (IntV 2)) (UnaryApp (UnaryApp (Var "+") (Var "x")) (Var "y"))))
--
-- >>> parse parseExp "" "(let [x 1 y 2] (if true x y))"
-- Right (Let (Var "x") (Lit (IntV 1)) (Let (Var "y") (Lit (IntV 2)) (If (Lit (BoolV True)) (Var "x") (Var "y"))))
--
parseExp :: Parser Exp
parseExp =
  try parseString <|>
  try parseBool <|>
  try parseInt <|>
  try parseList <|>
  try parseVector <|>
  try parseDef <|>
  try parseIf <|>
  try parseLet <|>
  try parseUnaryApp <|>
  try parseNullaryApp <|>
  try parseFun <|>
  -- try parseFun <|>
  try parseVar

-- | Parse a line of expression.
--
-- >>> parse parseLine "" "[+ - >>= abc-def 123]"
-- Right (List [Var "+",Var "-",Var ">>=",Var "abc-def",Lit (IntV 123)])
--
-- >>> isLeft $ parse parseLine "" "+ 13"
-- True
--
parseLine :: Parser Exp
parseLine = do
  expr <- parseExp

  -- Expect spaces and EOF after expression.
  _ <- many space
  _ <- eof

  return expr

-- | Parse a Neblen program.
--
-- >>> parseProgram "(+ 1 2)"
-- Right (UnaryApp (UnaryApp (Var "+") (Lit (IntV 1))) (Lit (IntV 2)))
--
-- >>> parseProgram "(fn [x y] (x y))"
-- Right (Fun [Var "x",Var "y"] (UnaryApp (Var "x") (Var "y")))
--
-- >>> isLeft $ parseProgram "+ 13"
-- True
parseProgram :: NeblenProgram -> Either ParseError Exp
parseProgram = parse parseLine ""

parseTString :: Parser Type
parseTString = string "String" >> return TString

parseTBool :: Parser Type
parseTBool = string "Bool" >> return TBool

parseTInt :: Parser Type
parseTInt = string "Int" >> return TInt

parseTFun :: Parser Type
parseTFun = parseListWithSurroundingPrefix (Just (string "->")) '(' ')' parseTypes TFun

parseTList :: Parser Type
parseTList = do
  _ <- char '['
  t <- parseType
  _ <- char ']'
  return (TList t)

parseTVar :: Parser Type
parseTVar = liftM TVar (many1 letter)

-- | Parse type.
--
-- >>> parse parseType "" "(-> a Int (-> a b [String] Bool) (-> Bool))"
-- Right (-> a Int (-> a b [String] Bool) (-> Bool))
--
parseType :: Parser Type
parseType =
  try parseTString <|>
  try parseTBool <|>
  try parseTInt <|>
  try parseTFun <|>
  try parseTList <|>
  try parseTVar

parseTypes :: Parser [Type]
parseTypes = parseMany parseType

--------------------------
-- Parse "kind world" types, which differs (for now) from "non-kind" types

-- | Parse type constant with unknown kind.
--
-- >>> parse parseTConst "" "Tree"
-- Right Tree
--
-- >>> isLeft $ parse parseTConst "" "a"
-- True
--
-- >>> isLeft $ parse parseTConst "" "(Tree a)"
-- True
--
parseTConst :: Parser Type
parseTConst = do
  name <- parseUpperCasedString
  return (TConst name KUnknownInit)

-- | Parse type variable with unknown kind.
--
-- >>> parse parseTVarK "" "abc"
-- Right abc : k?
--
-- >>> isLeft $ parse parseTVarK "" "Abc"
-- True
--
parseTVarK :: Parser Type
parseTVarK = do
  tname <- many1 lower
  return (TVarK tname KUnknownInit)

-- | Parse type application with unknown kinds.
--
-- >>> parse parseTApp "" "(Tree a)"
-- Right (Tree a : k?)
--
-- >>> parse parseTApp "" "(Foo a b c)"
-- Right (((Foo a : k?) b : k?) c : k?)
--
-- >>> isLeft $ parse parseTApp "" "(a b)"
-- True
--
parseTApp :: Parser Type
parseTApp = do
  _ <- char '('
  ctor <- parseTConst
  _ <- spaces
  types <- parseMany parseTypeKind
  _ <- char ')'
  return (buildTAppStack (ctor:types))

buildTAppStack :: [Type] -> Type
buildTAppStack (lhs:[rhs]) = TApp lhs rhs
buildTAppStack (a:b:rest) = buildTAppStack (TApp a b:rest)
buildTAppStack _ = error "buildTAppStack requires at least two arguments"

parseTypeKind :: Parser Type
parseTypeKind = parseTConst <|> parseTVarK <|> parseTApp

