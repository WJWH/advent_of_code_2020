import qualified Data.IntMap as M

input = M.fromList $ zip [0,1,5,10,3,12] [1..]  -- skipping the 7th input number, 19

playGame :: Int -> M.IntMap Int -> Int -> Int -> Int
playGame cutoff memory round mostRecentlySpoken
  | round == cutoff = mostRecentlySpoken
  | otherwise = case M.lookup mostRecentlySpoken memory of
    Nothing -> playGame cutoff (M.insert mostRecentlySpoken round memory) (round + 1) 0
    Just k  -> playGame cutoff (M.insert mostRecentlySpoken round memory) (round + 1) (round - k)

part1 = playGame 2020 input 7 19

-- Takes a minute or so in the interpreter, but still gives the correct answer
-- We could try to optimize it by trying to detect a cycle and/or compiling with
-- O2 but eh.
part2 = playGame 30000000 input 7 19
