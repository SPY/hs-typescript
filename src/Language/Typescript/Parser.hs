module Language.Typescript.Parser (parseString) where

import Text.Parsec

import Language.Typescript.Types
import Language.Typescript.Parser.Statement (statement)
import Language.Typescript.Parser.Token (comment)

program = many1 $ statement <|> comment

parseString :: String -> Either ParseError Program
parseString = runParser program () ""
