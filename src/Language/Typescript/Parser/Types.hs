module Language.Typescript.Parser.Types where

data Statement = VarDefinition [VarDefinition]
               | FunDefinition (Maybe String) FunBody
               | Number Float
                 deriving (Show, Eq)

type VarDefinition = (String, Maybe Statement)
type FunBody = [Statement]

