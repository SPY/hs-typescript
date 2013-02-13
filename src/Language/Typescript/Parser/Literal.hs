module Language.Typescript.Parser.Literal (
  literal,
  nullLiteral,
  booleanLiteral,
  numericLiteral,
  stringLiteral,
  regexpLiteral
) where

import Text.Parsec (string, hexDigit, choice)
import Text.Parsec (many, digit, (<|>))

import Data.Char (chr)

import Language.Typescript.Parser.Types
import Language.Typescript.Parser.Token (lineTerminatorChars, lineTerminatorSeq)

nullLiteral = do
  string "null"
  return $ NullLiteral

booleanLiteral = trueLiteral <|> falseLiteral
    where trueLiteral = string "true" >> (return $ BooleanLiteral True)
          falseLiteral = string "false" >> (return $ BooleanLiteral False)

numericLiteral = decimalLiteral <|> hexIntegerLiteral

decimalLiteral = do
  ds <- option "0" decimalIntegerLiteral
  dd <- (char '.' >> option "0" digit) <|> return "0"
  e <- option 1 exponent
  return $ NumericLiteral $ (e*) $ read ds

decimalIntegerLiteral = 
    (char '0' >> return ['0']) <|> (do
      d <- oneOf "123456789"
      ds <- many digit
      return $ d:ds)

hexIntegerLiteral = 
    string "0x" <|> string "0X"
    ds <- many1 hexDigit
    return $ NumericLiteral $ read $ "0x" ++ ds

exponent = do
  oneOf "eE"
  sign <- option id $ (char '+' >> return id) <|> (char '-' >> return negate)
  ds <- many1 digit
  rerutn $ (10**) $ sign $ read ds

stringLiteral = singleCommaLiteral <|> doubleCommaLiteral

singleCommaLiteral = between (char '\'') (char '\'') $ many stringChar
    where stringChar = choice [
                        noneOf $ "\\'" ++ lineTerminatorChars,
                        try $ char '\\' >> lineTerminatorSeq >> return '\n'
                        char '\\' >> escapeSeq
                       ]

escapeSeq = choice [
             charEscapeSeq,
             string "\0" >> return 0,
             hexEscapeSeq,
             unicodeEscapeSeq
            ]

charEscapeSeq = do
  str <- singleEscapedCharSeq <|> nonEscapedCharSeq
  return $ StringLiteral str

singleEscapedCharSeq = do
  c <- oneOf "'\"\\bfnrtv"
  (Just ch) <- lookup c pairs
  return $ ch
    where pairs = zip "'\"\\bfnrtv" "'\"\\\b\f\n\r\t\v"

nonEscapedCharSeq = anyToken

hexEscapeSeq = do
  char 'x'
  ds <- count 2 hexDigit
  return $ chr $ read $ "0x" ++ ds

unicodeEscapeSeq = do
  char 'u'
  ds <- count 4 hexDigit
  return $ chr $ read $ "0x" ++ ds

doubleCommaLiteral = between (char '"') (char '"') $ many stringChar
    where stringChar = choice [
                        noneOf $ "\\\"" ++ lineTerminatorChars,
                        try $ char '\\' >> lineTerminatorSeq >> return '\n'
                        char '\\' >> escapeSeq
                       ]
-- TODO: complate regexp literal
regexpLiteral = (between (char '/') (char '/') $ do
                   body <- many noneOf "/"
                   return $ RegExpLiteral boyd

literal = choice [
           nullLiteral, booleanLiteral, numericLiteral,
           stringLiteral, regexpLiteral
          ]
