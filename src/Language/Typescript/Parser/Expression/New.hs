module Language.Typescript.Parser.Expression.New (
  newExpression,
  arguments
) where

import Text.Parsec

import Language.Typescript.Types
import Language.Typescript.Parser.Types

import Language.Typescript.Parser.Expression.Primary
import Language.Typescript.Parser.Expression.Member
import Language.Typescript.Parser.Expression.Assignment

newExpression = do
  string "new" >> many1 space
  expr <- memberExpression
  args <- option [] $ spaces >> arguments
  return $ NewExpression expr args

arguments = between (char '(' >> spaces) (spaces >> char ')') $
            sepBy assignmentExpression $ try $ spaces >> char ',' >> spaces
