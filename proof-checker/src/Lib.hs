module Lib where

import Text.Parsec
import Text.Parsec.String (Parser)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

---create a parser for the <number>
Number :: Parser String
Number = many1 digit

numberParser :: Parser Int
numberParser = fmap read Number

---create a parser for the <const-expr>
Letter :: Parser Char
Letter = upper

letterParser :: Parser Char
letterParser = fmap read Letter

---create a parser for the <conjunction-expr>
data ConjunctionExpr = ConjunctionExpr Expr Expr

conjunctionExprParser :: Parser ConjunctionExpr

conjunctionExprParser = do
  _         <- string "("
  _         <- string "and"
  _         <- spaces
  expr1  <- conjunctionExprParser
  _         <- spaces
  expr2  <- conjunctionExprParser
  _         <- string ")"
  return (ConjunctionExpr expr1 expr2)

---create a parser for the <conditional-expr>
data ConditionalExpr = ConditionalExpr Expr Expr

conditionalExprParser :: Parser ConditionalExpr

conditionalExprParser = do
  _         <- string "("
  _         <- string "if"
  _         <- spaces
  expr1  <- conditionalExprParser
  _         <- spaces
  expr2  <- conditionalExprParser
  _         <- string ")"
  return (ConditionalExpr expr1 expr2)

---create a parser for the <expr>
data Expr = ConditionalExpr | ConjunctionExpr | Letter
  deriving (Show)

exprParser :: Parser Expr

---create a parser for the <conjunction-elim-rule>
data ConjunctionElimRule = ConjunctionElimRule Expr Number

conjunctionElimRuleParser :: Parser ConjunctionElimRule

conjunctionElimRuleParser = do
  _         <- string "&E"
  _         <- spaces
  expr1  <- exprParser
  _         <- spaces
  num1  <- numberParser
  return (ConjunctionElimRule expr1 num1)

---create a parser for the <conjunction-intro-rule>
data ConjunctionIntroRule = ConjunctionIntroRule ConjunctionExpr Number Number

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
data ConditionalElimRule = ConditionalElimRule Expr Number Number

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

conditionalIntroRuleParser :: Parser ConditionalIntroRule

conditionalIntroRuleParser = do
  _         <- string "->I"
  _         <- spaces
  expr1  <- conditionalExprParser
  return (ConditionalIntroRule expr1)

---create a parser for the <non-discharge-rule>
data NonDischargeRule = ConditionalElimRule | ConjunctionIntroRule | ConjunctionElimRule
  deriving (Show)

nonDischargeRuleParser :: Parser NonDischargeRule

conditionalIntroRuleParser = do
  _         <- string "("
  expr1  <- nonDischargeRuleParser
  _         <- string ")"
  return (NonDischargeRule expr1)

---create a parser for the <discharge-rule>
data DischargeRule = ConditionalIntroRule
  deriving (Show)

dischargeRuleParser :: Parser DischargeRule

---create a parser for the <hypothesis>
data Hypothesis = Hypothesis Number Expr

hypothesisParser :: Parser Hypothesis

hypothesisParser = do
  _         <- newline
  num1  <- numberParser
  _         <- spaces
  _         <- string "("
  _         <- string "hyp"
  _         <- spaces
  expr1  <- exprParser
  _         <- string ")"
  return (Hypothesis num1 expr1)

---create a parser for the <derivation-line>
data DerivationLine = DerivationLine Number Subproof
                    | DerivationLine Number NonDischargeRule

derivationLineParser :: Parser DerivationLine

derivationLineParser = do
  _         <- newline
  num1  <- numberParser
  _         <- spaces
  expr1  <- try subproofParser <|> try nonDischargeRuleParser
  return (DerivationLine num1 expr1)

---create a parser for the <subproof>
data Subproof = Subproof DischargeRule Hypothesis [DerivationLine]

subproofParser :: Parser Subproof

subproofParser = do
  _         <- string "("
  expr1  <- dischargeRuleParser
  _         <- newline
  _         <- string "("
  _         <- string "proof"
  list1  <- many1 derivationLineParser
  _         <- string ")"
  _         <- string ")"
  return (Subproof expr1 list1)

---create a parser for the <proof>
data Proof = Proof Number Subproof

proofParser :: Parser Proof

proofParser = do
  num1  <- numberParser
  _         <- spaces
  expr1  <- subproofParser
  return (Proof num1 expr1)