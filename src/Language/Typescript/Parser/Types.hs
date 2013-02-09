module Language.Typescript.Parser.Types where

data Statement = SVarDefinition [VarDefinition]
               | SFunDefinition (Maybe String) FunBody
               | SNumber Float
               | SString String
                 deriving (Show, Eq)

type VarDefinition = (String, Maybe Statement)
type FunBody = [Statement]

