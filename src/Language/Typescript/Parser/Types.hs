module Language.Typescript.Parser.Types where

data Statement = SVarDefinition [VarDefinition]
               | SFunDefinition (Maybe String) FunBody [Argument]
               | SNumber Float
               | SString String
               | SObject [Field]
                 deriving (Show, Eq)

type VarDefinition = (String, Maybe Statement)
type FunBody = [Statement]
type Argument = String
type Field = (String, Statement)
