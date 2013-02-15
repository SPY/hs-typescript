module Language.Typescript.Parser.Literal.String (stringLiteral) where

import Text.Parsec

import Data.Char (chr)

import Language.Typescript.Types
import Language.Typescript.Parser.Types

import Language.Typescript.Parser.Token (lineTerminatorChars, lineTerminatorSeq)

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
