module Lisp.Parser where

import Lisp.Types
import Text.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

skip :: Parser a -> Parser ()
skip m = m >> pure ()

parseString :: Parser Expr
parseString = do
  skip $ char '"'
  x <- many (noneOf "\"")
  skip $ char '"'
  return $ EStr x

parseVariable :: Parser Expr
parseVariable = do
  first <- letter <|> symbol
  rest <- many (letter <|> symbol <|> digit)
  pure $ EVar $ first:rest

parseNumber :: Parser Expr
parseNumber = (EInt . read) <$> many1 digit

parseList :: Parser Expr
parseList = do
  skip $ char '('
  x <- many parseExpr
  skip $ many space
  skip $ char ')'
  return $ foldr (:.:) ENil x

parseExpr :: Parser Expr
parseExpr = do
  skip $ many space
  parseNumber <|> parseString <|> parseVariable <|> parseList

tryParse :: String -> Either ParseError Expr
tryParse input = parse parseExpr "lisp" input
