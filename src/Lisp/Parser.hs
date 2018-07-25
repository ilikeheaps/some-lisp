module Lisp.Parser where

import Lisp.Types
import Text.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~."

skip :: Parser a -> Parser ()
skip m = m >> pure ()

parseString :: Parser Expr
parseString = do
  skip $ char '"'
  x <- many (noneOf "\"")
  skip $ char '"'
  return $ EStr x

parseBool :: Parser Expr
parseBool = parseTrue <|> parseFalse
  where parseTrue :: Parser Expr
        parseTrue = string "true" >> pure (EBool True)
        parseFalse :: Parser Expr
        parseFalse = string "false" >> pure (EBool False)

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
  skip $ many space
  x <- many $ skipTrailing space parseExpr
  skip $ char ')'
  return $ foldr (:.:) ENil x

parseExpr :: Parser Expr
parseExpr = parseNumber <|> parseString <|> parseBool <|> parseVariable <|> parseList

skipTrailing :: Parser b -> Parser a -> Parser a
skipTrailing s p = do
  x <- p
  skip $ many s
  pure x

tryParse :: String -> Either ParseError Expr
tryParse input = parse parseExpr "lisp" input
