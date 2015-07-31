{-# LANGUAGE OverloadedStrings #-}

module Neblen.Parser where

import Neblen.Data
import Text.ParserCombinators.Parsec
import qualified Control.Applicative as A
import qualified Data.Set as S

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Either

-- | Skip one or more spaces.
skipSpaces1 :: Parser ()
skipSpaces1 = skipMany1 space

-- | Parse string.
--
-- >>> parse parseString "" "\"Hello\""
-- Right (Literal (StringV "Hello"))
--
-- >>> parse parseString "" "\"\""
-- Right (Literal (StringV ""))
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
  return $ Literal (StringV s)

-- | Parse Boolean.
--
-- >>> parse parseBool "" "true"
-- Right (Literal (BoolV True))
--
-- >>> parse parseBool "" "false"
-- Right (Literal (BoolV False))
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
  return $ Literal (BoolV (s == "true"))

-- | Parse Integers.
--
-- >>> parse parseInt "" "0"
-- Right (Literal (IntV 0))
--
-- >>> parse parseInt "" "123"
-- Right (Literal (IntV 123))
--
-- >>> parse parseInt "" "123.456"
-- Right (Literal (IntV 123))
--
-- >>> isLeft $ parse parseInt "" ".456"
-- True
--
parseInt :: Parser Exp
parseInt = do
  s <- many1 digit
  return $ Literal (IntV (read s :: Int))

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
-- Right (List [Var "xyz-abc",Literal (StringV "abc"),Literal (IntV 123)])
--
-- >>> parse parseList "" "(list xyz-abc (list 0 \"foo\" true))"
-- Right (List [Var "xyz-abc",List [Literal (IntV 0),Literal (StringV "foo"),Literal (BoolV True)]])
--
-- >>> parse parseList "" "(list (list 0 \"foo\" true))"
-- Right (List [List [Literal (IntV 0),Literal (StringV "foo"),Literal (BoolV True)]])
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
-- Right (List [Literal (IntV 1),Var "abc-xyz",Literal (StringV "abc")])
--
parseList' :: Parser Exp
parseList' = try (string "(list)") A.*> A.pure (List []) <|> try (parseListWithSurroundingPrefix (Just (string "list")) '(' ')' List)

-- | Parse vectors (which are just lists with different syntax, for now).
--
-- >>> parse parseVector "" "[]"
-- Right (List [])
--
-- >>> parse parseVector "" "[xyz-abc [0 \"foo\" true]]"
-- Right (List [Var "xyz-abc",List [Literal (IntV 0),Literal (StringV "foo"),Literal (BoolV True)]])
--
parseVector :: Parser Exp
parseVector = parseListWithSurrounding '[' ']' List

parseExps :: Parser [Exp]
parseExps = sepBy parseExp skipSpaces1

parseListWithSurroundingPrefix :: Maybe (Parser String) -> Char -> Char -> ([Exp] -> Exp) -> Parser Exp
parseListWithSurroundingPrefix mp l r f = do
  _ <- char l
  case mp of
    Just s -> s A.*> skipSpaces1
    _      -> spaces

  -- Must be separated by at least one space
  exps <- parseExps

  _ <- char r
  return $ f exps

parseListWithSurrounding :: Char -> Char -> ([Exp] -> Exp) -> Parser Exp
parseListWithSurrounding = parseListWithSurroundingPrefix Nothing

-- | Parse definition.
--
-- >>> parse parseDef "" "(def x 123)"
-- Right (Def (Var "x") (Literal (IntV 123)))
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
-- Right (UnaryApp (Var "x") (Literal (IntV 123)))
--
-- Curry (x 1 2) as ((x 1) 2):
--
-- >>> parse parseUnaryApp "" "(x 1 2)"
-- Right (UnaryApp (UnaryApp (Var "x") (Literal (IntV 1))) (Literal (IntV 2)))
--
-- >>> parse parseUnaryApp "" "(x 1 2 3)"
-- Right (UnaryApp (UnaryApp (UnaryApp (Var "x") (Literal (IntV 1))) (Literal (IntV 2))) (Literal (IntV 3)))
--
-- >>> parse parseUnaryApp "" "((fn [x y z] (+ x y)) 1 2 3)"
-- Right (UnaryApp (UnaryApp (UnaryApp (Fun (Var "x") (Fun (Var "y") (Fun (Var "z") (UnaryApp (UnaryApp (Var "+") (Var "x")) (Var "y"))))) (Literal (IntV 1))) (Literal (IntV 2))) (Literal (IntV 3)))
--
parseUnaryApp :: Parser Exp
parseUnaryApp = try $ do
  _ <- char '('
  varOrFn <- parseVar <|> parseFun
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
-- >>> buildAppStack (Var "x") [Literal (IntV 1)]
-- UnaryApp (Var "x") (Literal (IntV 1))
--
-- >>> buildAppStack (Var "x") [Literal (IntV 1),Literal (IntV 2),Literal (IntV 3)]
-- UnaryApp (UnaryApp (UnaryApp (Var "x") (Literal (IntV 1))) (Literal (IntV 2))) (Literal (IntV 3))
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
-- >>> parse parseNullaryApp "" "(list)"
-- Right (NullaryApp (Var "list"))
--
-- >>> isLeft $ parse parseNullaryApp "" "()"
-- True
--
parseNullaryApp :: Parser Exp
parseNullaryApp = do
  _ <- char '('
  varOrFn <- parseVar
  _ <- char ')'
  return $ NullaryApp varOrFn

-- | Parse functions.
--
-- >>> parse parseFun "" "(fn [x] (+ x 123))"
-- Right (Fun (Var "x") (UnaryApp (UnaryApp (Var "+") (Var "x")) (Literal (IntV 123))))
--
-- >>> parse parseFun "" "(fn [x y z] (x y z))"
-- Right (Fun (Var "x") (Fun (Var "y") (Fun (Var "z") (UnaryApp (UnaryApp (Var "x") (Var "y")) (Var "z")))))
--
parseFun :: Parser Exp
parseFun = do
  parseStartsListWith "fn"
  argsVec <- parseVecOfVars
  body <- parseBodyOfFun
  return $ buildFunCurry body argsVec

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

buildFunCurry :: Exp -> [Exp] -> Exp
buildFunCurry body [] = NullaryFun body
buildFunCurry body [Var v] = Fun (Var v) body
buildFunCurry body (Var v:as) = Fun (Var v) (buildFunCurry body as)
buildFunCurry _ _ = error "Invalid function argument list."

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
-- Right (Let (Var "x") (Literal (IntV 1)) (Let (Var "y") (Literal (IntV 2)) (UnaryApp (UnaryApp (Var "+") (Var "x")) (Var "y"))))
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
-- Right [(Var "x",Literal (IntV 1)),(Var "y",Literal (IntV 2))]
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

-- | Parse expression.
--
-- >>> parse parseExp "" "\"abc\""
-- Right (Literal (StringV "abc"))
--
-- >>> parse parseExp "" "true"
-- Right (Literal (BoolV True))
--
-- >>> parse parseExp "" "123"
-- Right (Literal (IntV 123))
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
-- Right (Def (Var "x") (Literal (IntV 123)))
--
-- >>> parse parseExp "" "(foo bar)"
-- Right (UnaryApp (Var "foo") (Var "bar"))
--
-- >>> parse parseExp "" "(foo)"
-- Right (NullaryApp (Var "foo"))
--
-- >>> parse parseExp "" "(let [x 1 y 2] (+ x y))"
-- Right (Let (Var "x") (Literal (IntV 1)) (Let (Var "y") (Literal (IntV 2)) (UnaryApp (UnaryApp (Var "+") (Var "x")) (Var "y"))))
--
parseExp :: Parser Exp
parseExp =
  try parseString <|>
  try parseBool <|>
  try parseInt <|>
  try parseList <|>
  try parseVector <|>
  try parseDef <|>
  try parseUnaryApp <|>
  try parseNullaryApp <|>
  try parseFun <|>
  try parseLet <|>
  try parseVar

-- | Parse a line of expression.
--
-- >>> parse parseLine "" "[+ - >>= abc-def 123]"
-- Right (List [Var "+",Var "-",Var ">>=",Var "abc-def",Literal (IntV 123)])
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
-- Right (UnaryApp (UnaryApp (Var "+") (Literal (IntV 1))) (Literal (IntV 2)))
--
-- >>> parseProgram "(fn [x] x)"
-- Right (Fun (Var "x") (Var "x"))
--
-- >>> isLeft $ parseProgram "+ 13"
-- True
parseProgram :: NeblenProgram -> Either ParseError Exp
parseProgram = parse parseLine ""


