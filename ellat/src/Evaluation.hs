module Evaluation where

import Type

toBool :: Bool -> BaseType
toBool True = 1
toBool False = 0

fromBool :: BaseType -> Bool
fromBool 0 = False
fromBool _ = True

binaryOperatorEvaluation :: BinaryOperator -> BaseType -> BaseType -> BaseType
binaryOperatorEvaluation Add x y = x + y
binaryOperatorEvaluation Sub x y = x - y
binaryOperatorEvaluation Mul x y = x * y
binaryOperatorEvaluation Div x y = x `div` y
binaryOperatorEvaluation Lt x y = toBool (x < y)
binaryOperatorEvaluation Le x y = toBool (x <= y)
binaryOperatorEvaluation Gt x y = toBool (x > y)
binaryOperatorEvaluation Ge x y = toBool (x >= y)
binaryOperatorEvaluation Eq x y = toBool (x == y)
binaryOperatorEvaluation Neq x y = toBool (x /= y)
binaryOperatorEvaluation And x y = toBool (fromBool x && fromBool y)
binaryOperatorEvaluation Or x y = toBool (fromBool x || fromBool y)

updateEnvironment :: Environment -> VariableName -> BaseType -> Environment
updateEnvironment [] _ _ = undefined
updateEnvironment ((name, current_value) : next) variable_name value =
    if variable_name == name then
        [(name, value)] ++ next
    else
        [(name, current_value)] ++ (updateEnvironment next variable_name value)

getValue :: Environment -> VariableName -> BaseType
getValue [] _ = undefined
getValue ((name, value) : next) variable_name =
    if variable_name == name then
        value
    else
        getValue next variable_name

---------- expressionEvaluation ----------
expressionEvaluation :: Environment -> Expression -> BaseType
expressionEvaluation environment (Literal value) = value
expressionEvaluation environment (Variable name) = getValue environment name
expressionEvaluation environment (BinaryOperation operator x y) = binaryOperatorEvaluation operator x' y'
    where
        x' = expressionEvaluation environment x
        y' = expressionEvaluation environment y

---------- statementEvaluation ----------
statementEvaluation :: Environment -> Statement -> Environment
statementEvaluation environment (DeclareVariable variable_name initial_value)
    = [(variable_name, initial_value')] ++ environment
        where initial_value' = expressionEvaluation environment initial_value

statementEvaluation environment (Sequence statement1 statement2)
    = statementEvaluation environment' statement2
        where environment' = statementEvaluation environment statement1

statementEvaluation environment (Assign variable_name value)
    = updateEnvironment environment variable_name value'
        where value' = expressionEvaluation environment value

statementEvaluation environment (If condition then_statement else_statement)
    = if expressionEvaluation environment condition == 0 then
          statementEvaluation environment else_statement
      else
          statementEvaluation environment then_statement

statementEvaluation environment Pass = environment

statementEvaluation environment (While condition statement)
    = if expressionEvaluation environment condition == 0 then
          environment
      else
          statementEvaluation environment' (While condition statement)
              where environment' = statementEvaluation environment statement