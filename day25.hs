{-# LANGUAGE BangPatterns #-}

pubkeyDoor = 6930903
pubkeyCard = 19716708

transform :: Integer -> Integer -> Integer
transform !base 0 = 1
transform !base 1 = base
transform !base n
  | even n    =        transform ((base * base) `mod` 20201227) ( n    `div` 2)
  | otherwise = (base * transform ((base * base) `mod` 20201227) ((n-1) `div` 2)) `mod` 20201227 

findLoopSize :: Integer -> Integer -> Integer -> Integer
findLoopSize !loopsSoFar target !acc
  | acc == target = loopsSoFar
  | otherwise = findLoopSize (loopsSoFar + 1) target $ (acc * 7) `mod` 20201227

part1 = transform pubkeyDoor cardLoops
  where cardLoops = findLoopSize 0 pubkeyCard 1

-- SPOILER:
-- There is no part 2 today.
