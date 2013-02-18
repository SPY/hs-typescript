module Language.Typescript.Parser.Expression where

import Text.Parsec ((<|>), try)

import Language.Typescript.Types

import Language.Typescript.Parser.Expression.Primary
import Language.Typescript.Parser.Expression.Member
import Language.Typescript.Parser.Expression.New

expression = try newExpression <|> memberExpression
