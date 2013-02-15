module Language.Typescript.Parser.Literal (
  literal,
  nullLiteral,
  booleanLiteral,
  numericLiteral,
  stringLiteral,
  regexpLiteral
) where

import Text.Parsec

import Data.Char (chr)

import Language.Typescript.Types
import Language.Typescript.Parser.Types
import Language.Typescript.Parser.Token (lineTerminatorChars, lineTerminatorSeq)

nullLiteral :: TSParser Literal
nullLiteral = do
  string "null"
  return $ Null

booleanLiteral :: TSParser Literal
booleanLiteral = trueLiteral <|> falseLiteral
    where trueLiteral = string "true" >> (return $ Boolean True)
          falseLiteral = string "false" >> (return $ Boolean False)

numericLiteral :: TSParser Literal
numericLiteral = decimalLiteral <|> hexIntegerLiteral

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
    string "0x" <|> string "0X"
    ds <- many1 hexDigit
    return $ Numeric $ read $ "0x" ++ ds

exponentPart :: TSParser Double
exponentPart = do
  oneOf "eE"
  sign <- option id $ (char '+' >> return id) <|> (char '-' >> return negate)
  ds <- many1 digit
  return $ (10**) $ sign $ read ds

stringLiteral = singleCommaLiteral <|> doubleCommaLiteral

singleCommaLiteral = between (char '\'') (char '\'') $ do
  cs <- many stringChar
  return $ StringLiteral cs
    where stringChar = choice [
                        noneOf $ "\\'" ++ lineTerminatorChars,
                        try $ char '\\' >> lineTerminatorSeq >> return '\n',
                        char '\\' >> escapeSeq
                       ]

escapeSeq = choice [
             charEscapeSeq,
             string "\0" >> return (chr 0),
             hexEscapeSeq,
             unicodeEscapeSeq
            ]

charEscapeSeq = singleEscapedCharSeq <|> nonEscapedCharSeq

singleEscapedCharSeq = do
  c <- oneOf "'\"\\bfnrtv"
  let (Just ch) = lookup c pairs
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

doubleCommaLiteral = between (char '"') (char '"') $ do 
  cs <- many stringChar
  return $ StringLiteral cs
    where stringChar = choice [
                        noneOf $ "\\\"" ++ lineTerminatorChars,
                        try $ char '\\' >> lineTerminatorSeq >> return '\n',
                        char '\\' >> escapeSeq
                       ]
-- TODO: complate regexp literal
regexpLiteral = between (char '/') (char '/') $ do
  body <- many $ noneOf "/"
  return $ RegExp body

literal = nullLiteral <|> booleanLiteral <|> numericLiteral <|>
          stringLiteral <|> regexpLiteral
