import Utils
import qualified Data.IntMap as M
import Data.Maybe

data Rule = Stop | Terminal Char | OtherRules { refs :: [[Int]] } deriving (Show,Eq)
type RuleMap = M.IntMap Rule

terminal :: Parser Rule
terminal = do
  char '"'
  c <- letter -- really just a or b but eh
  char '"'
  return $ Terminal c

-- abusing input structure a bit: in the real input there are never more than two
-- rules in the list
listOfRule :: Parser [Int]
listOfRule = do
  i1 <- integer
  i2 <- optionMaybe $ try $ do
    char ' '
    integer
  return $ catMaybes [Just i1, i2]

otherRules :: Parser Rule
otherRules = do
  rs <- listOfRule `sepBy` string " | "
  return $ OtherRules rs

rule :: Parser (Int,Rule)
rule = do
  i <- integer
  string ": "
  r <- choice [terminal, otherRules]
  return (i,r)

input :: Parser (M.IntMap Rule, [String])
input = do
  rules <- rule `endBy` endOfLine
  endOfLine
  messages <- (many letter) `sepBy` endOfLine
  eof
  return (M.fromList rules, messages)

matchRules :: RuleMap -> [Rule] -> String -> Bool
-- Matching the empty string is only valid with a Stop rule and no further rules
matchRules rm [Stop] "" = True
matchRules rm [Stop] ms = False
matchRules rm rs     "" = False
matchRules rm ((Terminal c):rs) (m:ms) = if c == m then matchRules rm rs ms else False
matchRules rm ((OtherRules rs):rss) (m:ms) = any (\r -> matchRules rm ((rules r)++rss) (m:ms)) rs
  where rules rs = map (\r -> rm M.! r) rs

updateRulemapWithStopRule :: RuleMap -> RuleMap
updateRulemapWithStopRule rm = rm''
  where rm' = M.insert (-1) Stop rm
        ruleZero = rm M.! 0
        rm'' = M.adjust (\_ -> OtherRules [((head $ refs ruleZero) ++ [-1])]) 0 rm'

part1 = do
  Right (rulemap', messages) <- parseFile input "day19_input.txt"
  let rulemap = updateRulemapWithStopRule rulemap'
  print . length . filter (== True) $ map (\m -> (matchRules rulemap [rulemap M.! 0] m)) messages

part2 = do
  Right (rulemap', messages) <- parseFile input "day19_input.txt"
  let rulemap = updateRulemapWithStopRule rulemap'
  let updatedRulemap = M.insert 8 (OtherRules [[42],[42,8]]) $ M.insert 11 (OtherRules [[42,31],[42,11,31]]) rulemap
  print . length . filter (== True) $ map (\m -> (matchRules updatedRulemap [updatedRulemap M.! 0] m)) messages
