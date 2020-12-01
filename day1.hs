import qualified Data.IntSet as S

-- Nice O(N) solution
part1 = do
  nums <- (foldr S.insert S.empty . map read . lines <$> readFile "day1_input.txt") :: IO (S.IntSet)
  let filteredSet = S.filter (\n -> (2020 - n) `S.member` nums) nums
  print . product . S.toList $ filteredSet

-- Brute force but eh there's only 200 lines in the input
part2 = do
  nums <- map read . lines <$> readFile "day1_input.txt" :: IO [Int]
  print [ x*y*z | x <- nums, y <- nums, z <- nums, x < y, y < z, x+y+z == 2020]

-- $> part1 >> part2
