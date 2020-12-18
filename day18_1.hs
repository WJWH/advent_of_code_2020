{-# LANGUAGE OverloadedStrings #-}
import Utils
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Operator = MulOp | AddOp deriving (Show,Eq)
data Expression = Literal Int
                | Operation Operator Expression Expression
                deriving (Show,Eq)

literal :: Parser Expression
literal = do
  n <- integer
  return $ Literal n

mulOp :: Parser Operator
mulOp = do
  char '*'
  return MulOp

addOp :: Parser Operator
addOp = do
  char '+'
  return AddOp

operator :: Parser Expression
operator = do
  n1 <- choice [bracketExpression, literal]
  spaces
  op <- choice [mulOp, addOp]
  spaces
  n2 <- expression
  return $ Operation op n1 n2

bracketExpression :: Parser Expression
bracketExpression = do
  char '('
  exp <- expression
  char ')'
  return exp

expression :: Parser Expression
expression = choice [try operator, literal, bracketExpression]

inputFile :: Parser [Expression]
inputFile = do
  exprs <- expression `endBy` endOfLine
  eof
  return exprs

eval :: Expression -> Int
eval (Literal n) = n
eval (Operation MulOp x y) = (eval x) * (eval y)
eval (Operation AddOp x y) = (eval x) + (eval y)

-- Weird hack because the problem statement wants folding "from the left" while for some
-- reason I don't understand right now my parser generates a format only appropriate for
-- folding from the right.
reverseLineKeepingBrackets :: T.Text -> T.Text
reverseLineKeepingBrackets ts = T.replace "~" "(" . T.replace "(" ")" . T.replace ")" "~" $ T.reverse ts

part1 = do
  input <- T.unlines . map reverseLineKeepingBrackets . T.lines <$> TIO.readFile "day18_input.txt"
  let Right results = parse inputFile "input" input
  print . sum $ map eval results
