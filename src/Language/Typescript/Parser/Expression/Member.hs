module Language.Typescript.Parser.Expression.Member (
  memberExpression
) where

import Text.Parsec

import Language.Typescript.Types
import Language.Typescript.Parser.Types

import {-# SOURCE #-} Language.Typescript.Parser.Expression (expression)
import Language.Typescript.Parser.Expression.Primary

dotRest nest = do
  spaces >> char '.' >> spaces
  ident <- identifier
  let newNest = Dot nest ident
  option newNest $ memberRest newNest

bracketsRest nest = do
  spaces >> char '[' >> spaces
  expr <- expression
  spaces >> char ']'
  let newNest = Brackets nest expr
  option newNest $ memberRest newNest

memberRest nest = choice $ map ($ nest) [dotRest, bracketsRest]

restMemberExpr simpleExpr = do
  let simple = SimpleMember simpleExpr
  expr <- try $ memberRest simple
  return $ MemberExpression expr

memberExpression = do
  simpleExpr <- primaryExpression
  restMemberExpr simpleExpr <|> return simpleExpr
