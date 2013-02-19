module Language.Typescript.Parser.Expression.Call (
  callExpression
) where

import Text.Parsec

import Language.Typescript.Types
import Language.Typescript.Parser.Types

import Language.Typescript.Parser.Expression.Member
import Language.Typescript.Parser.Expression.New (arguments)

callExpression = do
  expr <- memberExpression
  spaces
  args <- arguments
  return $ CallExpression expr args
