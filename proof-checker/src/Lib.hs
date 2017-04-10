-- @Author Kyle Zeller
-- @Author Zach Sabin

module Lib where

import Text.Parsec
import Text.Parsec.String (Parser)
import System.IO  

----------------------------------------------------
-- Main Functions -----------------------------------
----------------------------------------------------

data Result = Result {success :: Bool, message :: String} deriving (Show)

-- Reads in a file containing a proof, parses the proof, and returns whether or not the proof was valid
main = do  
    putStr "Enter a fileName: "
    fileName <- getLine 
    proofString <- readFile fileName 
    let result = checkProof (parse proofParser "" proofString)
    putStrLn (message result)
    if (success result)
      then putStrLn "-- Proof is valid --"
      else putStrLn "-- Proof is invalid --"

checkProof :: Either ParseError DerivationTree -> Result
checkProof (Right proof) = Result (validateProof proof) "Parse completed successfully"
checkProof (Left error) = Result False ("Parse error: " ++ (show error))

----------------------------------------------------
-- Data Structures ---------------------------------
----------------------------------------------------

data Operator = If | And deriving (Eq, Show)

data ExprTree = Const { val :: [Char] } 
    | Expr  { op :: Operator, 
            arg1 :: ExprTree, 
            arg2 :: ExprTree } deriving (Eq, Show) 

data NDRule = CondElim | ConjIntro | ConjElim deriving (Eq, Show)

data DerivationTree =
    Proof { condIntro :: ExprTree, 
          hyp :: ExprTree, 
          derivations :: [DerivationTree] }
    | DerivationLine { rule :: NDRule,
            statement :: ExprTree, 
            lines :: [Int] } deriving (Show)


----------------------------------------------------
-- Parser Functions --------------------------------
----------------------------------------------------

---create a parser for the <number>
numberStringParser :: Parser String
numberStringParser = many1 digit

numberParser :: Parser Int
numberParser = fmap read numberStringParser

---create a parser for the <const-expr>
constParser :: Parser ExprTree
constParser = do 
    exp <- (many (noneOf "() "))
    return (Const exp)

---create a parser for the <conjunction-expr>
conjunctionExprParser :: Parser ExprTree
conjunctionExprParser = do
  _         <- string "("
  _         <- string "and"
  _         <- spaces
  expr1  <- exprParser
  _         <- spaces
  expr2  <- exprParser
  _         <- string ")"
  return (Expr And expr1 expr2)

---create a parser for the <conditional-expr>
conditionalExprParser :: Parser ExprTree
conditionalExprParser = do
  _         <- string "("
  _         <- string "if"
  _         <- spaces
  expr1  <- exprParser
  _         <- spaces
  expr2  <- exprParser
  _         <- string ")"
  return (Expr If expr1 expr2)

---create a parser for the <expr>
exprParser :: Parser ExprTree
exprParser = (try conditionalExprParser) <|> (try conjunctionExprParser) <|> constParser

---create a parser for the <conjunction-elim-rule>
conjunctionElimRuleParser :: Parser DerivationTree
conjunctionElimRuleParser = do
  _         <- string "&E"
  _         <- spaces
  expr1  <- exprParser
  _         <- spaces
  num1  <- numberParser
  return (DerivationLine ConjElim expr1 [num1])

---create a parser for the <conditional-elim-rule>
conditionalElimRuleParser :: Parser DerivationTree
conditionalElimRuleParser = do
  _         <- string "->E"
  _         <- spaces
  expr1  <- exprParser
  _         <- spaces
  num1  <- numberParser
  _         <- spaces
  num2  <- numberParser
  return (DerivationLine CondElim expr1 [num1, num2])

---create a parser for the <conjunction-intro-rule>
conjunctionIntroRuleParser :: Parser DerivationTree
conjunctionIntroRuleParser = do
  _         <- string "&I"
  _         <- spaces
  expr1  <- conjunctionExprParser
  _         <- spaces
  num1  <- numberParser
  _         <- spaces
  num2  <- numberParser
  return (DerivationLine ConjIntro expr1 [num1, num2])

nonDischargeRuleParser :: Parser DerivationTree
nonDischargeRuleParser = do
  _         <- string "("
  expr1  <- (try conditionalElimRuleParser) <|> (try conjunctionIntroRuleParser) <|> conjunctionElimRuleParser
  _         <- string ")"
  return (expr1)

dischargeRuleParser :: Parser ExprTree
dischargeRuleParser = do
  _         <- string "->I"
  _         <- spaces
  expr1  <- conditionalExprParser
  return (expr1)

---create a parser for the <hypothesis>
hypothesisParser :: Parser ExprTree
hypothesisParser = do
  _         <- newline
  num1  <- numberParser
  _         <- spaces
  _         <- string "("
  _         <- string "hyp"
  _         <- spaces
  expr1  <- exprParser
  _         <- string ")"
  return (expr1)

---create a parser for the <derivation-line>
derivationLineParser :: Parser DerivationTree
derivationLineParser = do
  _         <- newline
  num1  <- numberParser
  _         <- spaces
  expr1  <- (try subproofParser) <|> nonDischargeRuleParser
  return (expr1)

---create a parser for the <subproof>
subproofParser :: Parser DerivationTree
subproofParser = do
  _         <- string "("
  expr1  <- dischargeRuleParser
  _         <- newline
  _         <- string "("
  _         <- string "proof"
  hyp    <- hypothesisParser
  list1  <- many1 derivationLineParser
  _         <- string ")"
  _         <- string ")"
  return (Proof expr1 hyp list1)

---create a parser for the <proof>
proofParser :: Parser DerivationTree
proofParser = do
  num1  <- numberParser
  _         <- spaces
  subproof  <- subproofParser
  return (subproof)

-----------------------------------------------------------------------------
-- Proof Validation Functions -----------------------------------------------
-----------------------------------------------------------------------------

validateProof :: DerivationTree -> Bool
validateProof proof = validateDerivationLine (buildExprList [] [proof]) proof

validateDerivationLine :: [ExprTree] -> DerivationTree -> Bool
validateDerivationLine exprList (Proof intro hyp derivations) = (validateCondIntro intro hyp derivations) && (validateDerivationList exprList derivations)
validateDerivationLine exprList (DerivationLine rule statement lines) = validateNDRule rule statement (getExpressions exprList lines)

validateDerivationList :: [ExprTree] -> [DerivationTree] -> Bool
validateDerivationList exprList (x:xs) = (validateDerivationLine exprList x) && (validateDerivationList exprList xs)
validateDerivationList _ [] = True

-- CondIntro: If you have hypothesized P at the beginning of a (proof or) subproof, and in that subproof have derived Q, 
-- then you may leave that subproof and state that P -> Q.
validateCondIntro :: ExprTree -> ExprTree -> [DerivationTree] -> Bool
validateCondIntro (Expr If p q) hyp derivations = (p == hyp) && (derivesExpr derivations q)
validateCondIntro expr hyp derivations = False

-- CondElim: If both P->Q and P are True, then Q is True.
-- ConjIntro: If both P and Q are True, then P & Q is True.
-- ConjElim: If P & Q is True, then P is True. If P & Q is True, then Q is True.
validateNDRule :: NDRule -> ExprTree -> [ExprTree] -> Bool
validateNDRule CondElim q1 [(Expr If p2 q2), p1] = (p1 == p2) && (q1 == q2) 
validateNDRule ConjIntro (Expr And p2 q2) [p1, q1] = ((p1 == p2) && (q1 == q2)) || ((p1 == q2) && (q1 == p2))
validateNDRule ConjElim x [(Expr And p q)] = (x == p) || (x == q)
validateNDRule _ _ _ = False

derivesExpr :: [DerivationTree] -> ExprTree -> Bool
derivesExpr ((Proof intro _ derivations):xs) expr = intro == expr || (derivesExpr derivations expr) || (derivesExpr xs expr)
derivesExpr ((DerivationLine _ statement _):xs) expr = statement == expr || (derivesExpr xs expr)
derivesExpr [] _ = False

buildExprList :: [ExprTree] -> [DerivationTree] -> [ExprTree]
buildExprList list (x:xs) = buildExprList (list ++ (toExpr x)) xs
buildExprList list [] = list

toExpr :: DerivationTree -> [ExprTree]
toExpr (Proof intro hyp derivations) = (buildExprList [intro, hyp] derivations)
toExpr (DerivationLine _ statement _) = [statement]

getExpressions :: [ExprTree] -> [Int] -> [ExprTree]
getExpressions exprList (x:xs) = (exprList !! (x - 1)):(getExpressions exprList xs)
getExpressions exprList [] = []