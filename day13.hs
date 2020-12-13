import Data.List
import Data.List.Split
import Data.Ord

n = 1000340
inputs = "13,x,x,x,x,x,x,37,x,x,x,x,x,401,x,x,x,x,x,x,x,x,x,x,x,x,x,17,x,x,x,x,19,x,x,x,23,x,x,x,x,x,29,x,613,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,41"

part1 = do
  let runningBuses = map read . filter (/= "x") $ splitOn "," inputs :: [Int]
  print . (\(x,y) -> x*y) . head . sortBy (comparing snd) $ map (\x -> (x,((n `div` x)+1) * x - n)) runningBuses

addBus :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
addBus (start,step) (offset,busNr) = (head matchingTimes, head $ tail matchingTimes)
  where matchingTimes = [ x | x <- [start,step..], (x + offset) `mod` busNr == 0] 

part2 = do
  let runningBuses = map (\(x,y) -> (x, read y)) . filter (\(_,y) -> y /= "x") . zip [0..] $ splitOn "," inputs :: [(Integer,Integer)]
  print . fst $ foldl' addBus (1,2) runningBuses
