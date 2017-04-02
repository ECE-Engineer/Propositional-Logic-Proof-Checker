module Lib where

import Text.Parsec
import Text.Parsec.String (Parser)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

numberStringParser :: Parser String
numberStringParser = many1 digit

numberParser :: Parser Int
numberParser = fmap read numberStringParser

data Operator = Add | Subtract
  deriving (Show)

readOperator :: String -> Operator
readOperator "+" = Add
readOperator "-" = Subtract
readOperator _   = error "Undefined operator"

addParser :: Parser String
addParser = string "+"
subtractParser :: Parser String
subtractParser = string "-"

operatorStringParser :: Parser String
operatorStringParser = addParser <|> subtractParser

operatorParser :: Parser Operator
operatorParser = fmap readOperator operatorStringParser

data Expression = Literal Int | Operation Expression Operator Expression
  deriving (Show)

literalParser :: Parser Expression
literalParser = fmap Literal numberParser

operationParser :: Parser Expression
operationParser = do
  _         <- string "("
  firstExp  <- expressionParser
  operator  <- operatorParser
  secondExp  <- expressionParser
  _         <- string ")"
  let expression = Operation firstExp operator secondExp
  return expression

expressionParser :: Parser Expression
expressionParser = operationParser <|> literalParser
------------------------------------------------------------evaluation of expressions
evaluate :: Expression -> Int
evaluate (Literal n) = n

evaluate (Operation expr1 op expr2) = performOperation op (evaluate expr1) (evaluate expr2)
performOperation :: Operator -> Int -> Int -> Int
performOperation Add a b = a + b
performOperation Subtract a b = a - b

evaluationParser :: Parser Int
evaluationParser = fmap evaluate expressionParser
