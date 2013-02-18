module Language.Typescript.Parser.Expression.Assignment (
  assignmentExpression
) where

import Text.Parsec

import Language.Typescript.Types
import Language.Typescript.Parser.Types

import {-# SOURCE #-} Language.Typescript.Parser.Expression (expression)

assignmentExpression = expression
