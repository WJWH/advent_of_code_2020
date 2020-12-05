import Data.List

parseFB :: String -> Int
parseFB = foldl' (\acc n -> (acc*2) + if n == 'B' then 1 else 0) 0

parseLR :: String -> Int
parseLR = foldl' (\acc n -> (acc*2) + if n == 'R' then 1 else 0) 0

parseLocation :: String -> (Int,Int)
parseLocation s = (parseFB front, parseLR back)
  where (front,back) = splitAt 7 s

part1 = do
  locations <- map parseLocation . lines <$> readFile "day5_input.txt"
  print . maximum $ map (\(x,y) -> (x*8)+y) locations

findHole :: [Int] -> Int
findHole [] = error "didn't find any hole in the list"
findHole (x:[]) = error "didn't find any hole in the list"
findHole (x:y:xs) = case x + 1 == y of
  False -> x+1
  True  -> findHole (y:xs)

part2 = do
  locations <- map parseLocation . lines <$> readFile "day5_input.txt"
  print . findHole . sort $ map (\(x,y) -> (x*8)+y) locations
