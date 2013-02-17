module Language.Typescript.Parser.Expression where

import Text.Parsec ((<|>))

import Language.Typescript.Types

import Language.Typescript.Parser.Expression.Primary
import Language.Typescript.Parser.Expression.Member

expression = memberExpression <|> primaryExpression
