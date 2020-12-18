import Utils

data Operator = MulOp | AddOp deriving (Show,Eq)
data Expression = Literal Int
                | Operation Operator Expression [Expression]
                deriving (Show,Eq)

literal :: Parser Expression
literal = do
  n <- integer
  return $ Literal n

bracketExpression :: Parser Expression
bracketExpression = do
  char '('
  exp <- expression
  char ')'
  return exp

-- Straight from https://en.wikipedia.org/wiki/Operator-precedence_parser: 
-- expression ::= equality-expression
-- equality-expression ::= additive-expression ( ( '==' | '!=' ) additive-expression ) *
-- additive-expression ::= multiplicative-expression ( ( '+' | '-' ) multiplicative-expression ) *
-- multiplicative-expression ::= primary ( ( '*' | '/' ) primary ) *
-- primary ::= '(' expression ')' | NUMBER | VARIABLE | '-' primary

-- Only in this case we need to switch the order of multiplicative-expression and additive-expression
-- so that addition is more important. We can also leave out equality and / and - and variable
-- assignments ofc

expression = multiplicativeExpression

multiplicativeExpression = do
  e1 <- additiveExpression
  es <- many $ try $ do
    spaces
    char '*'
    spaces
    additiveExpression
  return $ Operation MulOp e1 es

additiveExpression = do
  e1 <- primary
  es <- many $ try $ do
    spaces
    char '+'
    spaces
    primary
  return $ Operation AddOp e1 es

primary = choice [bracketExpression, literal]

eval :: Expression -> Int
eval (Literal n) = n
eval (Operation MulOp x ys) = (eval x) * (product $ map eval ys)
eval (Operation AddOp x ys) = (eval x) + (sum $ map eval ys)

part2 = do
  Right results <- parseFileLines expression "day18_input.txt"
  print . sum $ map eval results
