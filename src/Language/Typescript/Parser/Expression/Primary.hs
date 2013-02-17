module Language.Typescript.Parser.Expression.Primary (
  identifier,
  this,
  primaryLiteral,
  parentheses,
  primaryExpression
) where

import Text.Parsec

import Control.Monad (guard)

import Language.Typescript.Types
import Language.Typescript.Parser.Types
import Language.Typescript.Parser.Literal
import Language.Typescript.Parser.Token (unicodeLetter, unicodeEscapedChar)
import Language.Typescript.Parser.Keyword

import {-# SOURCE #-} Language.Typescript.Parser.Expression (expression)

identifierName :: TSParser String
identifierName = do
  c <- initialChar
  cs <- many $ initialChar <|> restChar
  return $ c:cs
      where initialChar = oneOf "_$" <|> unicodeLetter <|> unicodeEscapedChar
            restChar = digit <|> oneOf "\x200C\x200D"

identifier :: TSParser Primary
identifier = do
  name <- identifierName
  guard . not $ name `elem` reservedWords
  return $ Identifier name

this :: TSParser Primary
this = string "this" >> return This

primaryLiteral :: TSParser Primary
primaryLiteral = do
  lit <- literal
  return $ PrimaryLiteral lit

parentheses :: TSParser Primary
parentheses = between (char '(' >> spaces) (spaces >> char ')') $ do
  expr <- expression
  return $ Parentheses expr

primaryExpression :: TSParser Expression
primaryExpression = do
  prim <- this <|> primaryLiteral <|> identifier <|> parentheses
  return $ PrimaryExpression prim
