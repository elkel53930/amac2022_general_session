module Type where

type VariableName = String
type BaseType = Int

type Environment = [(VariableName, BaseType)]

data BinaryOperator
    = Add | Sub | Mul | Div
    | Lt | Le | Gt | Ge | Eq | Neq
    | And | Or
        deriving Show

data Expression
    = Literal BaseType
    | Variable VariableName
    | BinaryOperation BinaryOperator Expression Expression
        deriving Show

data Statement 
    = DeclareVariable VariableName Expression
    | Sequence Statement Statement
    | Assign VariableName Expression
    | If Expression Statement Statement
    | Pass
    | While Expression Statement
        deriving Show
