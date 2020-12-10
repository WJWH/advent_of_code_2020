import Data.List
import Control.Monad.State
import Data.Vector ((!))
import qualified Data.IntMap as M
import qualified Data.Vector as V

part1 = do
  sortedNums <- ((:) 0) . sort . (map read) . lines <$> readFile "day10_input.txt" :: IO [Int]
  let deltas = (zipWith (-) (tail sortedNums) sortedNums) ++ [3] -- weird extra requirement
  print . (\(x,y) -> x*y) $ ((length $ filter (==1) deltas), (length $ filter (==3) deltas))

combinations :: V.Vector Int -> Int -> State (M.IntMap Integer) Integer
combinations v 0 = return 1
combinations v n = do
  state <- get
  case M.lookup n state of
    Just n  -> return n
    Nothing -> do
      let currentValue = v ! n
      let candidates = [ x | x <- [n-1,n-2,n-3], x >= 0, v ! x >= (currentValue - 3)] -- no more than 3 difference
      results <- sum <$> mapM (combinations v) candidates
      modify $ M.insert n results
      return results

part2 = do
  sortedNums <- V.fromList . (\xs -> xs ++ [(last xs) + 3]) . ((:) 0) . sort . (map read) . lines <$> readFile "day10_input.txt" :: IO (V.Vector Int)
  print $ evalState (combinations sortedNums (V.length sortedNums - 1)) M.empty
