module Language.Typescript.Parser.Literal.Numeric (numericLiteral) where

import Text.Parsec

import Language.Typescript.Types
import Language.Typescript.Parser.Types

numericLiteral :: TSParser Literal
numericLiteral =  try hexIntegerLiteral <|> decimalLiteral

decimalLiteral :: TSParser Literal
decimalLiteral = do
  lookAhead $ digit <|> (char '.' >> digit) 
  ds <- option "0" decimalIntegerLiteral
  dd <- (char '.' >> option "0" (many1 digit)) <|> return "0"
  e <- option 1 exponentPart
  return $ Numeric $ (e*) $ read (ds ++ "." ++ dd)

decimalIntegerLiteral :: TSParser String
decimalIntegerLiteral = 
    (char '0' >> return "0") <|> (do
      d <- oneOf "123456789"
      ds <- many digit
      return $ d:ds)

hexIntegerLiteral :: TSParser Literal
hexIntegerLiteral = do
    char '0' >> oneOf "xX"
    ds <- many1 hexDigit
    return $ Numeric $ read $ "0x" ++ ds

exponentPart :: TSParser Double
exponentPart = do
  oneOf "eE"
  sign <- option id $ (char '+' >> return id) <|> (char '-' >> return negate)
  ds <- many1 digit
  return $ (10**) $ sign $ read ds
