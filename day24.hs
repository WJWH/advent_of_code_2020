import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

data Step = West | East | NorthWest | NorthEast | SouthWest | SouthEast deriving (Show,Eq)

readSteps :: String -> [Step]
readSteps [] = []
readSteps ('w':xs) = West : readSteps xs
readSteps ('e':xs) = East : readSteps xs
readSteps ('n':'w':xs) = NorthWest : readSteps xs
readSteps ('n':'e':xs) = NorthEast : readSteps xs
readSteps ('s':'w':xs) = SouthWest : readSteps xs
readSteps ('s':'e':xs) = SouthEast : readSteps xs

-- Coordinate system: first number is west-east axis, east is positive
-- second number is nw -> se, se is positive
stepToVector :: Step -> (Int,Int)
stepToVector West = (-1,0)
stepToVector East = (1,0)
stepToVector NorthWest = (0,-1)
stepToVector NorthEast = (1,-1)
stepToVector SouthWest = (-1,1)
stepToVector SouthEast = (0,1)

vecPlus :: (Int,Int) -> (Int,Int) -> (Int,Int)
vecPlus (a,b) (c,d) = (a+c,b+d)

findDestination :: [Step] -> (Int,Int)
findDestination steps = foldl' vecPlus (0,0) $ map stepToVector steps

part1 = do
  inputs <- map (findDestination . readSteps) . lines <$> readFile "day24_input.txt"
  let destinationMap = foldl' (\acc k -> M.insertWith (+) k 1 acc) M.empty inputs
  print . M.size $ M.filter odd destinationMap

neighboringTiles :: (Int,Int) -> [(Int,Int)]
neighboringTiles (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x+1,y-1), (x-1,y+1), (x,y+1)]  

shouldRemainBlack :: S.Set (Int,Int) -> (Int,Int) -> Bool
shouldRemainBlack blackTiles (x,y) = not $ numSurroundingBlackTiles == 0 || numSurroundingBlackTiles > 2
  where neighbors = neighboringTiles (x,y)
        numSurroundingBlackTiles = length [ n | n <- neighbors, S.member n blackTiles]

shouldTurnBlack :: S.Set (Int,Int) -> (Int,Int) -> Bool
shouldTurnBlack blackTiles (x,y) = numSurroundingBlackTiles == 2
  where neighbors = neighboringTiles (x,y)
        numSurroundingBlackTiles = length [ n | n <- neighbors, S.member n blackTiles]

-- Like last time, we only have to watch black tiles and tiles next to black tiles
-- since a white tile that is not next to a black tile is in no danger of being flipped
flipTiles :: S.Set (Int,Int) -> S.Set (Int,Int)
flipTiles allBlackTiles = remainingTiles <> newTiles
  where allNeighboringTiles = S.fromList . concat . map neighboringTiles $ S.toList allBlackTiles
        allWhiteTiles = S.difference allNeighboringTiles allBlackTiles
        remainingTiles = S.filter (shouldRemainBlack allBlackTiles) allBlackTiles
        newTiles = S.filter (shouldTurnBlack allBlackTiles) allWhiteTiles

part2 = do
  inputs <- map (findDestination . readSteps) . lines <$> readFile "day24_input.txt"
  let destinationMap = foldl' (\acc k -> M.insertWith (+) k 1 acc) M.empty inputs
  let allStates = iterate flipTiles $ S.fromList . M.keys $ M.filter odd destinationMap
  print . S.size $ allStates !! 100
