module Language.Typescript.Parser.Expression.Unary (
  unaryExpression,
  postfixExpression
) where

import Text.Parsec

import Language.Typescript.Types
import Language.Typescript.Parser.Types

import Language.Typescript.Parser.Expression.Member

postfixInc = string "++" >> return PostfixIncrement

postfixDec = string "--" >> return PostfixDecrement

postfixRest expr = do
  op <- spaces >> (postfixInc <|> postfixDec)
  return $ PostfixExpression op expr  

postfixExpression = do
  expr <- leftHandSideExpression
  option expr $ try $ postfixRest expr

unaryOp = choice [
           string "delete" >> return UnaryDelete,
           string "void" >> return UnaryVoid,
           string "typeof" >> return UnaryTypeOf,
           string "++" >> return UnaryIncrement,
           string "--" >> return UnaryDecrement,
           char '+' >> return UnaryPlus,
           char '-' >> return UnaryMinus,
           char '~' >> return UnaryBinNot,
           char '!' >> return UnaryNot
          ]

unaryExpression = do
  op <- unaryOp
  expr <- try unaryExpression <|> postfixExpression
  return $ UnaryExpression op expr
