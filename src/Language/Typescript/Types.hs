module Language.Typescript.Types where 

type Program = [SourceElement]

data SourceElement = SEComment String
                   | SEStatement Statement
                     deriving (Show, Eq)

data Statement = ExpressionStatement Expression
               | Block [Statement]
               | VariableStatement [VarDeclaration]
               | EmptyStatement
               | IfStatement Expression Statement (Maybe Statement)
               | IterationStatement Iteration
               | ContinueStatement (Maybe Identifier)
               | BreakStatement (Maybe Identifier)
               | ReturnStatement (Maybe Expression)
               | WithStatement Expression Statement
               | LabelledStatement Identifier Statement
               | SwitchStatement -- TODO
               | ThrowStatement Expression
               | TryStatement Statement (Maybe Statement) (Maybe Statement)
               | DebuggerStatement
                 deriving (Show, Eq)

data Expression = ExpressionSeq [Expression]
                | AssignmentExpression AssignmentOp Expression Expression
                | ConditionalExpression Expression Expression Expression
                | LogicalExpression LogicalOp Expression Expression
                | BinaryExpression BinaryOp Expression Expression
                | UnaryExpression UnaryOp Expression
                | CallExpression Expression [Expression]
                | NewExpression Expression [Expression]
                | MemberExpression Member
                | PostfixExpression PostfixOp
                | PrimaryExpression Primary
                | FunctionExpression (Maybe Identifier) [Argument] [SourceElement]
                  deriving (Show, Eq)

data Primary = Identifier Identifier
             | This
             | PrimaryLiteral Literal
             | Parentheses Expression
               deriving (Show, Eq)

data Member = SimpleMember Primary
            | Dot Member String
            | Brackets Member Expression
              deriving (Show, Eq)

data VarDeclaration = Initialled Identifier Expression
                    | Empty Identifier
                      deriving (Show, Eq)

type Identifier = String

type Argument = Identifier

data Iteration = DoWhile Statement Expression
               | While Expression Statement
               | ForCond (Maybe Statement) (Maybe Expression) (Maybe Expression) Statement
               | ForEach Statement Expression Statement
                 deriving (Show, Eq)


data Literal = Null
             | Boolean Bool
             | Numeric Double
             | StringLiteral String
             | RegExp String
             | ArrayLiteral [Maybe Expression]
             | ObjectLiteral [Property]
               deriving (Show, Eq)

data Property = SimpleProperty PropertyName Expression
              | Getter PropertyName Expression
              | Setter PropertyName Expression
                deriving (Show, Eq)

type PropertyName = String

data AssignmentOp = Assignment
                  | MultAssignment
                  | DivAssignment
                  | ModAssignment
                  | PlusAssignment
                  | MinusAssignment
                  | LeftShiftAssignment
                  | RightShiftAssignment
                  | RightCycleShiftAssignment
                  | BitAndAssignment
                  | BitXorAssignment
                  | BitOrAssignment
                    deriving (Show, Eq)

data LogicalOp = LogicalMore
               | LogicalMoreEq
               | LogicalEq
               | LogicalNotEq
               | LogicalStrictEq
               | LogicalStrictNotEq
               | LogicalLess
               | LogicalLessEq
               | LogicalInstanceOf
               | LogicalIn
               | LogicalOr
               | LogicalAnd
                 deriving (Show, Eq)

data BinaryOp = BinaryMult
              | BinaryDiv
              | BinaryMod
              | BinaryPlus
              | BinaryMinus
              | BinaryLeftShift
              | BinaryRightShift
              | BinaryRightCycleShift
              | BinaryBitAnd
              | BinaryBitOr
              | BinaryBitXor
                deriving (Show, Eq)

data UnaryOp = UnaryDelete
             | UnaryVoid
             | UnaryTypeOf
             | UnaryIncrement
             | UnaryDecrement
             | UnaryPlus
             | UnaryMinus
             | UnaryBitNot
             | UnaryNot
               deriving (Show, Eq)

data PostfixOp = PostfixIcrement
               | PostfixDecrement
                 deriving (Show, Eq)

