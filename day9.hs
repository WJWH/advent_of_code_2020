import Utils
import Data.Sequence hiding (splitAt)
import Data.Foldable

prefixLength = 25

findFirstNonsum :: [Int] -> Int
findFirstNonsum xs = if head end `elem` [ x+y | x <- start, y <- start, x /= y ]
  then findFirstNonsum (tail xs)
  else head end
  where (start,end) = splitAt prefixLength xs

part1 = do
  Right nums <- parseFileLines integer "day9_input.txt"
  print $ findFirstNonsum nums

target = 258585477 -- this was the answer to part 1 for my input

findTarget :: Seq Int -> Int -> [Int] -> (Int,Int)
findTarget current acc (x:xs)
  | acc == target = (minimum current, maximum current)
  -- we overshot, remove one item from the head of the sequence and try again
  | acc >  target = findTarget rest (acc - first) (x:xs)
  -- current set does not sum to acc yet, add the next element from the list to the Seq
  | acc <  target = findTarget (current |> x) (acc + x) xs
    where (first :< rest) = viewl current

part2 = do
  Right nums <- parseFileLines integer "day9_input.txt"
  print . (\(x,y) -> x+y) $ findTarget empty 0 nums

-- $> part2