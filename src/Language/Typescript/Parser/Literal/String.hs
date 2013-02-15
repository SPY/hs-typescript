module Language.Typescript.Parser.Literal.String (stringLiteral) where

import Text.Parsec

import Data.Char (chr)

import Language.Typescript.Types
import Language.Typescript.Parser.Types

import Language.Typescript.Parser.Token (lineTerminatorChars, lineTerminatorSeq)

stringLiteral :: TSParser Literal
stringLiteral = singleCommaLiteral <|> doubleCommaLiteral

singleCommaLiteral :: TSParser Literal
singleCommaLiteral = between (char '\'') (char '\'') $ do
  cs <- many stringChar
  return $ StringLiteral cs
    where stringChar = choice [
                        noneOf $ "\\'" ++ lineTerminatorChars,
                        try $ char '\\' >> lineTerminatorSeq >> return '\n',
                        char '\\' >> escapeSeq
                       ]

escapeSeq :: TSParser Char
escapeSeq = choice [
             string "\0" >> return (chr 0),
             hexEscapeSeq,
             unicodeEscapeSeq,
             charEscapeSeq
            ]

charEscapeSeq :: TSParser Char
charEscapeSeq = singleEscapedCharSeq <|> nonEscapedCharSeq

singleEscapedCharSeq :: TSParser Char
singleEscapedCharSeq = do
  c <- oneOf "'\"\\bfnrtv"
  let (Just ch) = lookup c pairs
  return $ ch
    where pairs = zip "'\"\\bfnrtv" "'\"\\\b\f\n\r\t\v"

nonEscapedCharSeq :: TSParser Char
nonEscapedCharSeq = anyToken

hexEscapeSeq :: TSParser Char
hexEscapeSeq = do
  char 'x'
  ds <- count 2 hexDigit
  return $ chr $ read $ "0x" ++ ds

unicodeEscapeSeq :: TSParser Char
unicodeEscapeSeq = do
  char 'u'
  ds <- count 4 hexDigit
  return $ chr $ read $ "0x" ++ ds

doubleCommaLiteral :: TSParser Literal
doubleCommaLiteral = between (char '"') (char '"') $ do 
  cs <- many stringChar
  return $ StringLiteral cs
    where stringChar = choice [
                        noneOf $ "\\\"" ++ lineTerminatorChars,
                        try $ char '\\' >> lineTerminatorSeq >> return '\n',
                        char '\\' >> escapeSeq
                       ]
