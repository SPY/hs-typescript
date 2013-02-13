module Language.Typescript.Parser.Types where

data Token = Comment String
           | Identifier String
           | NullLiteral
           | BooleanLiteral Bool
           | NumericLiteral Float
           | StringLiteral String
           | RegExpLiteral String
           | This
           | ArrayLiteral [Maybe Expression]
           | ObjectLiteral [ObjectField]
           | ExpressionToken Expression
             deriving (Show, Eq)

data Expression = Primary Token
                  deriving (Show, Eq)

data ObjectField = Simple Token Expression
                 | Getter Token Token
                 | Setter Token Token
