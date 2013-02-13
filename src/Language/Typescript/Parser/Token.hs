module Language.Typescript.Parser.Token where

import Text.Parsec (oneOf, manyTill, string, anyToken, count, hexDigit)
import Text.Parsec (many, letter, digit)

import Control.Monad (guard)

import Language.Typescript.Parser.Keyword
import Language.Typescript.Parser.Types

lineTerminatorChars = "\LF\CR\x2028\x2029"
lineTerminator = oneOf lineTerminatorChars

lineTerminatorSeq = (string "\CR\LF" >> return '\n') <|> lineTerminator

comment = multiLineComment <|> singleLineComment

multiLineComment = do 
  string "/*"
  cs <- manyTill anyToken $ string "*/"
  return $ Comment cs

singleLineComment = do
  string "//"
  cs <- manyTill anyToken lineTerminator
  return $ Comment cs

-- TODO: add unicode categories 
unicodeLetter = letter

unicodeEscapedChar = do
  char '\\u'
  code <- count 4 hexDigit

identifierName = do
  c <- initialChar
  cs <- many initialChar <|> restChar
  return $ c:cs
      where initialChar = oneOf "_$" <|> unicodeLetter <|> unicodeEscapedChar
            restChar = digit <|> oneOf "\x200C\x200D"

identifier = do
  name <- identifierName
  guard . not $ name `elem` reservedWords
  return $ Identifier name

