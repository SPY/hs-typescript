module Language.Typescript.Parser.Keyword (
                                           reservedWords,
                                           keyword,
                                           reserved
                                          ) where

import Text.Parsec (string, choice)
import Data.Char (toUpper)

import Language.Typescript.Parser.Types

keyWords = [
 "break", "case", "catch", "continue", "debugger",
 "default", "delete", "do", "else", "finally", "for",
 "function", "if", "in", "instanceof", "new",
 "return", "switch", "this", "throw", "try", "typeof",
 "var", "void", "while", "with"
]

futureReservedWords = [
 "class", "const", "enum", "export", "extends",  "implements",
 "import", "interface", "let", "package", "private", 
 "protected", "public", "static", "super", "yield"
]

reservedWords = keyWords ++ futureReservedWords

parseKeyword (k, t) = do
  string k
  return t

keyword = choice $ map parseKeyword $ zip keyWords $ enumFrom $ toEnum 0 :: KeywordType

futureReserved = choice $ map parseKeyword $ 
                 zip futureReservedWords $ enumFrom $ toEnum 0 :: FutureReservedWord

reserved = keyword <|> futureReserved
