module Language.Typescript.Parser.Keyword (
  reservedWords,
  reserved
) where

import Text.Parsec (string, choice)

import Language.Typescript.Parser.Types

keyWords = [ "break", "case", "catch", "continue", "debugger",
             "default", "delete", "do", "else", "finally", "for",
             "function", "if", "in", "instanceof", "new",
             "return", "switch", "this", "throw", "try", "typeof",
             "var", "void", "while", "with"
           ]

futureReservedWords = [ "class", "const", "enum", "export", "extends", 
                        "implements", "import", "interface", "let", "package", "private", 
                        "protected", "public", "static", "super", "yield"
                      ]

reservedWords = keyWords ++ futureReservedWords

reserved :: TSParser String
reserved = choice $ map string reservedWords
