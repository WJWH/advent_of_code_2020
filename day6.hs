import Data.List.Split
import qualified Data.Set as S

part1 = do
  groups <- splitOn "\n\n" <$> readFile "day6_input.txt"
  print . sum $ map (S.size . S.fromList . concat . lines) groups

part2 = do
  groups <- splitOn "\n\n" <$> readFile "day6_input.txt"
  print . sum $ map (S.size . foldr1 S.intersection . map S.fromList . lines) groups
