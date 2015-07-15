{-# LANGUAGE OverloadedStrings #-}

module Neblen.Parser where

import Neblen.Data
import Text.ParserCombinators.Parsec

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Either

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
  s <- string "true" <|> string "false"
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
parseVar :: Parser Exp
parseVar = parseAlphaNumericId <|> parseSymbolId

-- | Parse an alpha-numeric identifier.
--
-- >>> parse parseVar "" "abc"
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
-- >>> isLeft $ parse parseAlphaNumericId "" "_"
-- True
--
-- >>> isLeft $ parse parseAlphaNumericId "" "123abc"
-- True
--
parseAlphaNumericId :: Parser Exp
parseAlphaNumericId = do
  prefix <- letter <|> char '-' <|> char '_' <?> "Non-symbolic identifier must start with a letter, dash or underscore."
  s <- many1 (alphaNum <|> char '-' <|> char '_') <?> "Non-symbolic identifier must consist of alphanumeric characters, dashes and underscores."
  return $ Var (prefix : s)

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
-- >>> parse parseList "" "(xyz-abc \"abc\" 123)"
-- Right (List [Var "xyz-abc",Literal (StringV "abc"),Literal (IntV 123)])
--
-- >>> parse parseList "" "(xyz-abc (0 \"foo\" true))"
-- Right (List [Var "xyz-abc",List [Literal (IntV 0),Literal (StringV "foo"),Literal (BoolV True)]])
--
-- >>> parse parseList "" "((0 \"foo\" true))"
-- Right (List [List [Literal (IntV 0),Literal (StringV "foo"),Literal (BoolV True)]])
--
-- >>> isLeft $ parse parseList "" "(123 456"
-- True
--
parseList :: Parser Exp
parseList = parseListWithSurrounding '(' ')' List

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

parseListWithSurrounding :: Char -> Char -> ([Exp] -> Exp) -> Parser Exp
parseListWithSurrounding l r f = do
  _ <- char l
  exps <- sepBy parseExp spaces
  _ <- char r
  return $ f exps

-- parseVec :: Parser [Exp]

-- | Parse expression.
--
-- >>> parse parseExp "" "\"abc\""
-- Right (Literal (StringV "abc"))
--
parseExp :: Parser Exp
parseExp = parseString <|> parseBool <|> parseInt <|> parseVar <|> parseList <|> parseVector

-- | Parse a Neblen program.
--
-- >>> parseProgram "(+ 1 2 3 4)"
-- Right (List [Var "+",Literal (IntV 1),Literal (IntV 2),Literal (IntV 3),Literal (IntV 4)])
--
parseProgram :: NeblenProgram -> Either ParseError Exp
parseProgram = parse parseExp ""

