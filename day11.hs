import qualified Data.Map as M
import Data.Maybe

data CellValue = Floor | EmptyChair | FullChair deriving (Show,Eq)

charToValue :: Char -> CellValue
charToValue '.' = Floor
charToValue 'L' = EmptyChair
charToValue '#' = FullChair

type GameMap = M.Map (Int,Int) CellValue

conway :: GameMap -> GameMap
conway old = M.mapWithKey singleCell old
  where cellsAroundCell (x,y) = [fromMaybe Floor (M.lookup pos old) | pos <- [(x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y),(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1)] ]
        singleCell pos EmptyChair = if 0 == (sum [ 1 | x <- cellsAroundCell pos, x == FullChair ]) then FullChair else EmptyChair
        singleCell pos FullChair  = if 3 <  (sum [ 1 | x <- cellsAroundCell pos, x == FullChair ]) then EmptyChair else FullChair
        singleCell _ Floor = Floor

countOccupiedSeats :: GameMap -> Int
countOccupiedSeats gm = length . filter (== FullChair) $ M.elems gm

part1 = do
  input <- lines <$> readFile "day11_input.txt"
  let height = length input
  let width = length $ head input
  let positions = [(x,y) | y <- [1..height], x <- [1..width]]
  let initialMap = M.fromList $ zip positions (map charToValue $ concat input)
  let allStates = iterate conway initialMap
  print . countOccupiedSeats . fst . head $ dropWhile (\(x,y) -> x /= y) (zip allStates (tail allStates))

tupleSum :: (Int,Int) -> (Int,Int) -> (Int,Int)
tupleSum (a,b) (c,d) = (a+c,b+d)

-- stores the positions of the first chair in each of the directions.
generateCheckmap :: GameMap -> M.Map (Int,Int) [(Int,Int)]
generateCheckmap gm = M.mapWithKey findFirstChairs gm
  where directions = [(-1,-1),(0,-1),(1,-1),(-1,0),(1,0),(-1,1),(0,1),(1,1)]
        findFirstChairs pos Floor = [] -- no need to do a lot of calculation here, people don't sit on the floor
        findFirstChairs pos _ = catMaybes $ map (firstChairInDirectionFrom pos) directions
        firstChairInDirectionFrom :: (Int,Int) -> (Int,Int) -> Maybe (Int,Int)
        firstChairInDirectionFrom from direction = let ts = (tupleSum from direction) in case M.lookup ts gm of
          Nothing -> Nothing -- outside map boundary
          Just EmptyChair -> Just ts
          Just Floor -> firstChairInDirectionFrom ts direction

conway2 :: M.Map (Int,Int) [(Int,Int)] -> GameMap -> GameMap
conway2 checkmap old = M.mapWithKey singleCell old
  where cellsAroundCell pos = [fromMaybe Floor (M.lookup pos old) | pos <- fromJust $ M.lookup pos checkmap]
        singleCell pos EmptyChair = if 0 == (sum [ 1 | x <- cellsAroundCell pos, x == FullChair ]) then FullChair else EmptyChair
        singleCell pos FullChair  = if 4 <  (sum [ 1 | x <- cellsAroundCell pos, x == FullChair ]) then EmptyChair else FullChair
        singleCell _ Floor = Floor

part2 = do
  input <- lines <$> readFile "day11_input.txt"
  let height = length input
  let width = length $ head input
  let positions = [(x,y) | y <- [1..height], x <- [1..width]]
  let initialMap = M.fromList $ zip positions (map charToValue $ concat input)
  let checkmap = generateCheckmap initialMap
  let allStates = iterate (conway2 checkmap) initialMap
  print . countOccupiedSeats . fst . head $ dropWhile (\(x,y) -> x /= y) (zip allStates (tail allStates))

-- $> part2