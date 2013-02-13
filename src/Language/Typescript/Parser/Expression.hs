module Language.Typescirpt.Parser.Expression where

import Text.Parsec ((<|>), string)

import Language.Typescript.Parser.Types
import Language.Typescript.Parser.Literal
import Language.Typescript.Parser.Token

primaryExpression = this <|> identifier <|> literal <|> arrayLiteral <|> objectLiteral

this = string "this" >> return This

arrayLiteral = between (char '[') (char ']') $ do
  elems <- many elem
  return $ ArrayLiteral elems
    where elem = emptyArrayElem <|> arrayElem
          emptyArrayElem = char ',' >> return Nothing

arrayElem = do
  expr <- assignmentExpression
  optional $ char ','
  return $ Just $ expr

objectLiteral = between (char '{') (char '}') $ do
  elems <- many objectElem
  return $ ObjectLiteral elems

propertyName = identifier <|> stringLiteral <|> numericLiteral

getter = do
  string "get" >> many1 space
  name <- propertyName
  spaces
  argumentsList
  funBody
  

objectElem = getter <|> setter <|> simpleProp

assignmentExpression = primaryExpression
