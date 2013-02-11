module Language.Typescript.Parser.Types where

data Token = Comment String
           | Identifier String
           | Keyword KeywordType
           | Literal LiteralType
             deriving (Show, Eq)

data KeywordType = Break
                 | Case
                 | Catch
                 | Continue
                 | Debugger
                 | Default
                 | Delete
                 | Do
                 | Else
                 | Finally
                 | For
                 | Function
                 | If
                 | In
                 | InstanceOf
                 | New
                 | Return
                 | Switch
                 | This
                 | Throw
                 | Try
                 | TypeOf
                 | Var
                 | Void
                 | While
                 | With
                   deriving (Show, Read, Eq, Enum)

data FutureReservedWord = Class
                         | Const
                         | Enum
                         | Export
                         | Extends
                         | Implements
                         | Import
                         | Interface
                         | Let
                         | Package
                         | Private
                         | Protected
                         | Public
                         | Static
                         | Super
                         | Yield
                           deriving (Show, Read, Eq, Enum)

data LiteralType = NullLiteral
                 | BooleanLiteral Bool
                 | NumericLiteral Float
                   deriving (Show, Eq)
