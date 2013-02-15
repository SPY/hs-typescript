module Language.Typescript.Parser.Literal (
  literal,
  nullLiteral,
  booleanLiteral,
  numericLiteral,
  stringLiteral,
  regexpLiteral
) where

import Text.Parsec

import Language.Typescript.Types
import Language.Typescript.Parser.Types

import Language.Typescript.Parser.Literal.Numeric
import Language.Typescript.Parser.Literal.String

nullLiteral :: TSParser Literal
nullLiteral = do
  string "null"
  return $ Null

booleanLiteral :: TSParser Literal
booleanLiteral = trueLiteral <|> falseLiteral
    where trueLiteral = string "true" >> (return $ Boolean True)
          falseLiteral = string "false" >> (return $ Boolean False)

-- TODO: complate regexp literal
regexpLiteral = between (char '/') (char '/') $ do
  body <- many $ noneOf "/"
  return $ RegExp body

literal = nullLiteral <|> booleanLiteral <|> numericLiteral <|>
          stringLiteral <|> regexpLiteral
