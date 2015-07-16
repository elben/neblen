{-# LANGUAGE OverloadedStrings #-}

module Neblen.Parser where

import Neblen.Data
import Text.ParserCombinators.Parsec
import qualified Control.Applicative as A

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
parseVar :: Parser Exp
parseVar = try parseAlphaNumericId <|> parseSymbolId

-- | Parse an alpha-numeric identifier.
--
-- >>> parse parseAlphaNumericId "" "abc"
-- Right (Var "abc")
--
-- >>> parse parseAlphaNumericId "" "_abc"
-- Right (Var "_abc")
--
-- >>> parse parseAlphaNumericId "" "-abc_def-123-"
-- Right (Var "-abc_def-123-")
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
parseAlphaNumericId :: Parser Exp
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
      return $ Var (p : p' : rest))
    else (do
      rest <- restOfAlphaNumericId
      return $ Var (p : rest))

restOfAlphaNumericId :: Parser String
restOfAlphaNumericId = many (alphaNum <|> char '-' <|> char '_') <?> "Non-symbolic identifier must consist of alphanumeric characters, dashes and underscores."

-- | Parse a symbol-only identifier.
--
-- >>> parse parseSymbolId "" ">= abc"
-- Right (Var ">=")
--
-- >>> parse parseSymbolId "" "+"
-- Right (Var "+")
--
-- >>> parse parseSymbolId "" "+++++"
-- Right (Var "+++++")
--
-- Bind operator.
-- >>> parse parseSymbolId "" ">>="
-- Right (Var ">>=")
--
-- >>> parse parseSymbolId "" "+3"
-- Right (Var "+")
--
-- >>> isLeft $ parse parseSymbolId "" "!!!"
-- True
--
parseSymbolId :: Parser Exp
parseSymbolId = do
  -- ID must be one or more symbols
  s <- many1 symbolIds
  return $ Var s

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

-- | Parse vectors.
--
-- >>> parse parseVector "" "[]"
-- Right (Vector [])
--
-- >>> parse parseVector "" "[xyz-abc [0 \"foo\" true]]"
-- Right (Vector [Var "xyz-abc",Vector [Literal (IntV 0),Literal (StringV "foo"),Literal (BoolV True)]])
--
parseVector :: Parser Exp
parseVector = parseListWithSurrounding '[' ']' Vector

parseListWithSurroundingPrefix :: Maybe (Parser String) -> Char -> Char -> ([Exp] -> Exp) -> Parser Exp
parseListWithSurroundingPrefix mp l r f = do
  _ <- char l
  case mp of
    Just s -> s A.*> skipSpaces1
    _      -> spaces
  exps <- sepBy parseExp (skipMany1 space)
  -- Must be separated by at least one space
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
-- >>> parse parseUnaryCall "" "(x 123)"
-- Right (UnaryCall (Var "x") (Literal (IntV 123)))
--
-- Curry (x 1 2) as ((x 1) 2):
--
-- >>> parse parseUnaryCall "" "(x 1 2)"
-- Right (UnaryCall (UnaryCall (Var "x") (Literal (IntV 1))) (Literal (IntV 2)))
--
-- >>> parse parseUnaryCall "" "(x 1 2 3)"
-- Right (UnaryCall (UnaryCall (UnaryCall (Var "x") (Literal (IntV 1))) (Literal (IntV 2))) (Literal (IntV 3)))
--
parseUnaryCall :: Parser Exp
parseUnaryCall = try $ do
  _ <- char '('
  varOrFn <- parseVar -- <|> parseFun
  skipSpaces1
  args <- many1 parseExp
  return $ buildCallStack varOrFn args

-- | Convert a function call with multiple arguments to recursive unary calls.
-- If no arguments are given, return a nullary function call.
--
-- >>> buildCallStack (Var "x") []
-- NullaryCall (Var "x")
--
-- >>> buildCallStack (Var "x") [Literal (IntV 1)]
-- UnaryCall (Var "x") (Literal (IntV 1))
--
-- >>> buildCallStack (Var "x") [Literal (IntV 1),Literal (IntV 2),Literal (IntV 3)]
-- UnaryCall (UnaryCall (UnaryCall (Var "x") (Literal (IntV 1))) (Literal (IntV 2))) (Literal (IntV 3))
--
buildCallStack :: Exp -> [Exp] -> Exp
buildCallStack fn [] = NullaryCall fn
buildCallStack fn [arg] = UnaryCall fn arg
buildCallStack fn (a:as) = buildCallStack (UnaryCall fn a) as

-- | Parse nullary function calls. That is, functions with no arguments.
--
-- >>> parse parseNullaryCall "" "(+)"
-- Right (NullaryCall (Var "+"))
--
-- >>> parse parseNullaryCall "" "(list)"
-- Right (NullaryCall (Var "list"))
--
-- >>> isLeft $ parse parseNullaryCall "" "()"
-- True
--
parseNullaryCall :: Parser Exp
parseNullaryCall = do
  _ <- char '('
  varOrFn <- parseVar
  _ <- char ')'
  return $ NullaryCall varOrFn

-- | Parse functions.
--
-- >>> parse parseList "" "(fn [x] (+ x 123))"
-- FIXME
--
parseFun :: Parser Exp
parseFun = parseList

-- | Parse expression.
--
-- >>> parse parseExp "" "\"abc\""
-- Right (Literal (StringV "abc"))
--
-- >>> parse parseExp "" "true"
-- Right (Literal (BoolV True))
--
-- >>> parse parseExp "" "x"
-- Right (Var "x")
--
-- >>> parse parseExp "" "[+ -]"
-- Right (Vector [Var "+",Var "-"])
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
-- Right (UnaryCall (Var "foo") (Var "bar"))
--
-- >>> parse parseExp "" "(foo)"
-- Right (NullaryCall (Var "foo"))
--
parseExp :: Parser Exp
parseExp = parseString <|> parseBool <|> parseInt <|> parseVar <|> parseList <|> parseVector <|> parseDef <|> parseUnaryCall <|> parseNullaryCall

-- | Parse a line of expression.
--
-- >>> parse parseLine "" "[+ - >>= abc-def 123]"
-- Right (Vector [Var "+",Var "-",Var ">>=",Var "abc-def",Literal (IntV 123)])
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
-- FIXME
--
-- >>> isLeft $ parseProgram "+ 13"
-- True
parseProgram :: NeblenProgram -> Either ParseError Exp
parseProgram = parse parseLine ""


