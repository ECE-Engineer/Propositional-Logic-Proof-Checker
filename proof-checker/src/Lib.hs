module Lib where

import Text.Parsec
import Text.Parsec.String (Parser)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

---create a parser for the <number>
numberStringParser :: Parser String
numberStringParser = many1 digit

numberParser :: Parser Int
numberParser = fmap read numberStringParser

---create a parser for the <const-expr>
letterStringParser :: Parser Char
letterStringParser = upper

---create a parser for the <conjunction-expr>
data ConjunctionExpr = ConjunctionExpr Expr Expr
  deriving (Show)

conjunctionExprParser :: Parser ConjunctionExpr

conjunctionExprParser = do
  _         <- string "("
  _         <- string "and"
  _         <- spaces
  expr1  <- exprParser
  _         <- spaces
  expr2  <- exprParser
  _         <- string ")"
  return (ConjunctionExpr expr1 expr2)

---create a parser for the <conditional-expr>
data ConditionalExpr = ConditionalExpr Expr Expr
  deriving (Show)

conditionalExprParser :: Parser ConditionalExpr

conditionalExprParser = do
  _         <- string "("
  _         <- string "if"
  _         <- spaces
  expr1  <- exprParser
  _         <- spaces
  expr2  <- exprParser
  _         <- string ")"
  return (ConditionalExpr expr1 expr2)

---create a parser for the <expr>
data Expr = Expr1 ConditionalExpr
          | Expr2 ConjunctionExpr
          | Expr3 Char
  deriving (Show)

exprParser :: Parser Expr

exprParser = try (fmap Expr1 conditionalExprParser) <|> try (fmap Expr2 conjunctionExprParser) <|> try (fmap Expr3 letterStringParser)

---create a parser for the <conjunction-elim-rule>
data ConjunctionElimRule = ConjunctionElimRule Expr Int
  deriving (Show)

conjunctionElimRuleParser :: Parser ConjunctionElimRule

conjunctionElimRuleParser = do
  _         <- string "&E"
  _         <- spaces
  expr1  <- exprParser
  _         <- spaces
  num1  <- numberParser
  return (ConjunctionElimRule expr1 num1)

---create a parser for the <conjunction-intro-rule>
data ConjunctionIntroRule = ConjunctionIntroRule ConjunctionExpr Int Int
  deriving (Show)

conjunctionIntroRuleParser :: Parser ConjunctionIntroRule

conjunctionIntroRuleParser = do
  _         <- string "&I"
  _         <- spaces
  expr1  <- conjunctionExprParser
  _         <- spaces
  num1  <- numberParser
  _         <- spaces
  num2  <- numberParser
  return (ConjunctionIntroRule expr1 num1 num2)

---create a parser for the <conditional-elim-rule>
data ConditionalElimRule = ConditionalElimRule Expr Int Int
  deriving (Show)

conditionalElimRuleParser :: Parser ConditionalElimRule

conditionalElimRuleParser = do
  _         <- string "->E"
  _         <- spaces
  expr1  <- exprParser
  _         <- spaces
  num1  <- numberParser
  _         <- spaces
  num2  <- numberParser
  return (ConditionalElimRule expr1 num1 num2)

---create a parser for the <conditional-intro-rule>
data ConditionalIntroRule = ConditionalIntroRule ConditionalExpr
  deriving (Show)

conditionalIntroRuleParser :: Parser ConditionalIntroRule

conditionalIntroRuleParser = do
  _         <- string "->I"
  _         <- spaces
  expr1  <- conditionalExprParser
  return (ConditionalIntroRule expr1)

---create a parser for the <non-discharge-rule>
data NonDischargeRule = NonDischargeRule1 ConditionalElimRule
                      | NonDischargeRule2 ConjunctionIntroRule
					  | NonDischargeRule3 ConjunctionElimRule
  deriving (Show)

nonDischargeRuleParser :: Parser NonDischargeRule

nonDischargeRuleParser = do
  _         <- string "("
  expr1  <- try (fmap NonDischargeRule1 conditionalElimRuleParser) <|> try (fmap NonDischargeRule2 conjunctionIntroRuleParser) <|> try (fmap NonDischargeRule3 conjunctionElimRuleParser)
  _         <- string ")"
  return (expr1)