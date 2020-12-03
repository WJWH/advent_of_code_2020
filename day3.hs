import qualified Data.Map as M

-- Definition:
-- X is sideways and grows to the right
-- Y is vertical and grows downwards

treesHitOnSlope :: M.Map (Int,Int) Char -> Int -> Int ->(Int,Int) -> Int
treesHitOnSlope m mapWidth mapHeight (dx,dy) = length . filter (== Just '#') $ map (\k -> M.lookup k m) allVisitedLocations
  where allVisitedLocations = takeWhile (\(x,y) -> y <= mapHeight) $ iterate (\(x,y) -> ((x+dx) `mod` mapWidth,y+dy)) (0,0)
  

part1 = do
  inputLines <- lines <$> readFile "day3_input.txt"
  let numLines = length inputLines
  let lineLength = length $ head inputLines
  let inputWithoutNewlines = concat inputLines
  let inputMap = foldr (\(value, coords) m -> M.insert coords value m) M.empty $ zip inputWithoutNewlines [ (x,y) | y <- [0..numLines-1], x <- [0..lineLength-1]]
  -- We can now look up coordinates in the inputMap to see if there is a tree or not 
  print $ treesHitOnSlope inputMap lineLength numLines (3,1)


part2 = do
  inputLines <- lines <$> readFile "day3_input.txt"
  let numLines = length inputLines
  let lineLength = length $ head inputLines
  let inputWithoutNewlines = concat inputLines
  let inputMap = foldr (\(value, coords) m -> M.insert coords value m) M.empty $ zip inputWithoutNewlines [ (x,y) | y <- [0..numLines-1], x <- [0..lineLength-1]]
  -- We can now look up coordinates in the inputMap to see if there is a tree or not 
  print . product $ map (treesHitOnSlope inputMap lineLength numLines) [(1,1),(3,1),(5,1),(7,1),(1,2)]

-- It would be easily doable with lookups into a list of lists from the input but I like the map better
-- and knowing AOC it'll be good to get some Map practice :)
