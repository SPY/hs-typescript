module Language.Typescript.Parser (parseString) where

import Text.Parsec
import Control.Monad (guard)

import Language.Typescript.Parser.Types

identifier = do
  c <- letter <|> oneOf "_$"
  cs <- many (alphaNum <|> oneOf "_$")
  return $ c:cs

varDef = try initedDef <|> emptyDef

emptyDef = do
  varName <- identifier
  return (varName, Nothing)

initedDef = do
  varName <- identifier
  spaces
  char '='
  spaces
  exp <- expression
  return (varName, Just exp)

var = do
  string "var"
  many1 space
  defs <- sepBy1 varDef (spaces >> char ',' >> spaces)
  return $ VarDefinition defs

number = try float <|> integer

float = do
  ds <- many digit
  char '.'
  dd <- many digit
  guard (length ds + length dd > 0)
  let int = if length ds > 0 then read ds else 0
      rat = if length dd > 0 then read ("0." ++ dd) else 0
  return $ Number $ int + rat

integer = do
  ds <- many1 digit
  return $ Number $ read ds 

expression = number

statement = var <|> expression

parseString :: String -> Either ParseError [Statement]
parseString  inp = runParser (many1 statement) () "" inp
