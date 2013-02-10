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
  return $ SVarDefinition defs

float = do
  ds <- many digit
  char '.'
  dd <- many digit
  guard (length ds + length dd > 0)
  let int = if length ds > 0 then read ds else 0
      rat = if length dd > 0 then read ("0." ++ dd) else 0
  return $ SNumber $ int + rat

scinific = do
  char 'e'
  sign <- negSign <|> posSign <|> (return id)
  (SNumber n) <- integer
  return $ (10 **) $ sign n
    where negSign = char '-' >> return negate
          posSign = char '+' >> return id

number = do
  (SNumber n) <- try float <|> integer
  (do  m <- scinific
       return $ SNumber $ n * m) <|> (return $ SNumber n)

integer = do
  ds <- many1 digit
  return $ SNumber $ read ds

stringChar quote = (string ['\\', quote] >> return quote) <|> noneOf [quote]

stringLiteral = do
  q <- char '\'' <|> char '"'
  str <- many $ stringChar q
  char q
  return $ SString str

function = do
  string "function"
  name <- optionMaybe (many1 space >> identifier)
  args <- params
  spaces
  body <- funBody
  return $ SFunDefinition name body args

params = 
    between (char '(') (char ')') $ 
            sepBy identifier (spaces >> char ',' >> spaces)

funBody = do
    char '{' >> spaces
    exps <- many statement
    spaces >> char '}'
    return exps

expression = function <|> number <|> stringLiteral

statement = var <|> expression

parseString :: String -> Either ParseError [Statement]
parseString  inp = runParser (many1 statement) () "" inp
