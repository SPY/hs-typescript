module Language.Typescript.Parser.Literal where

import Text.Parsec (string, hexDigit)
import Text.Parsec (many, digit)

import Control.Monad (guard)

import Language.Typescript.Parser.Types

nullLiteral = do
  string "null"
  return $ Literal NullLiteral

booleanLiteral = trueLiteral <|> falseLiteral
    where trueLiteral = string "true" >> (return $ Literal $ BooleanLiteral True)
          falseLiteral = string "false" >> (return $ Literal $ BooleanLiteral False)

numericLiteral = decimalLiteral <|> hexIntegerLiteral

decimalLiteral = do
  ds <- option "0" decimalIntegerLiteral
  dd <- (char '.' >> option "0" digit) <|> return "0"
  e <- option 1 exponent
  return $ Literal $ NumericLiteral $ (e*) $ read ds

decimalIntegerLiteral = 
    (char '0' >> return ['0']) <|> (do
      d <- oneOf "123456789"
      ds <- many digit
      return $ d:ds)

hexIntegerLiteral = 
    string "0x" <|> string "0X"
    ds <- many1 hexDigit
    return $ Literal $ NumericLiteral $ read $ "0x" ++ ds

exponent = do
  oneOf "eE"
  sign <- option id $ (char '+' >> return id) <|> (char '-' >> return negate)
  ds <- many1 digit
  rerutn $ (10**) $ sign $ read ds

