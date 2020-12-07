import Data.Graph.Inductive
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Query.DFS
import qualified Data.Map as M
import Utils

type BagColor = String

data BagRule = BagRule { from :: BagColor, contents :: [(BagColor, Int)]} deriving (Show,Eq)

singleBagColor :: Parser String
singleBagColor = do
  fromColor <- manyTill anyChar (try (string "bag"))
  optional $ char 's'
  return fromColor

bagAmountColor :: Parser (BagColor,Int)
bagAmountColor = do
  amount <- integer
  space
  bagColor <- singleBagColor
  return (bagColor, amount)

bagContents :: Parser [(BagColor,Int)]
bagContents = choice [
    string "no other bags" *> pure [],
    bagAmountColor `sepBy` string ", "
  ]

bagRule :: Parser BagRule
bagRule = do
  fromColor <- singleBagColor
  string " contain "
  contents <- bagContents
  char '.'
  return $ BagRule fromColor contents

constructEdges :: M.Map String Int -> BagRule -> [(Int,Int,Int)]
constructEdges nm br = map (\(dest,weight) -> (fromNode, safeLookup dest nm,weight)) $ contents br
  where fromNode = safeLookup (from br) nm

constructBagGraph :: [BagRule] -> Gr String Int
constructBagGraph bagRules = mkGraph nodes edges
  where nodes = zip [1..] $ map from bagRules
        -- reverse of `nodes`, for quicker lookup in edge construction
        nodeMap = M.fromList $ zip (map from bagRules) [1..]
        edges = concat $ map (constructEdges nodeMap) bagRules

part1 = do
  Right bagRules <- parseFileLines bagRule "day7_input.txt"
  let bagGraph = constructBagGraph bagRules
  let invertedGraph = grev bagGraph
  let shinyGoldNode = head [num | (num,name) <- labNodes bagGraph, name == "shiny gold "]
  -- tail because otherwise it also counts the shiny gold node itself
  print . length . tail $ reachable shinyGoldNode invertedGraph

recursiveCountBags :: Gr String Int -> Node -> Int
recursiveCountBags bg n = 1 + (sum $ zipWith (*) (map snd nextNodes) (map ((recursiveCountBags bg) . fst) nextNodes))
  where nextNodes = lsuc bg n

part2 = do
  Right bagRules <- parseFileLines bagRule "day7_input.txt"
  let bagGraph = constructBagGraph bagRules
  let shinyGoldNode = head [num | (num,name) <- labNodes bagGraph, name == "shiny gold "]
  -- In this case, also subtract 1 because recursiveCountBags counts the shiny gold
  -- bag but the assignment does not.
  print $ (recursiveCountBags bagGraph shinyGoldNode) - 1
