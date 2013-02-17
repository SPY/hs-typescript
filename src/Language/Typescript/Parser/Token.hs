module Language.Typescript.Parser.Token where

import Text.Parsec (oneOf, manyTill, string, anyToken, count, hexDigit)
import Text.Parsec (many, letter, digit, (<|>), Parsec)

import Control.Monad (guard)
import Data.Char (chr)

--import Language.Typescript.Parser.Keyword
import Language.Typescript.Types
import Language.Typescript.Parser.Types

lineTerminatorChars = "\LF\CR\x2028\x2029"
lineTerminator = oneOf lineTerminatorChars

lineTerminatorSeq = (string "\CR\LF" >> return '\n') <|> lineTerminator

comment = multiLineComment <|> singleLineComment

multiLineComment :: TSParser SourceElement
multiLineComment = do 
  string "/*"
  cs <- manyTill anyToken $ string "*/"
  return $ SEComment cs

singleLineComment :: TSParser SourceElement
singleLineComment = do
  string "//"
  cs <- manyTill anyToken lineTerminator
  return $ SEComment cs

-- TODO: add unicode categories 
unicodeLetter :: TSParser Char
unicodeLetter = letter

unicodeEscapedChar :: TSParser Char
unicodeEscapedChar = do
  string "\\u"
  code <- count 4 hexDigit
  return $ chr $ read $ "0x" ++ code
