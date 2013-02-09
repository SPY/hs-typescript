module Language.Typescript.Parser.Types where

data AST = VarDefinition [VarDefinition]
         deriving (Show, Eq)

type VarDefinition = (String, Maybe AST)

