import Data.Char
import Data.Maybe
import Data.Sequence as Seq hiding (sortBy,zip)
import Data.Foldable
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Unboxed as Vec
import Data.List
import Data.Ord
import GHC.Prim

input = map digitToInt "583976241" :: [Int]

playGame :: Seq Int -> Seq Int
playGame (current :<| rest) = (firstCirclePart >< pickedup >< (secondCirclePart |> current))
  where (pickedup,circle) = Seq.splitAt 3 rest
        findDestination 0 = findDestination 9
        findDestination n = case Seq.elemIndexL n pickedup of
          -- The index was not in the picked up set and must therefore be present in the circle
          Nothing -> n
          -- The index was in the picked up set, continue on with the next lower destination
          Just _  -> findDestination (n-1)
        destination = findDestination (current - 1)
        destinationIndex = fromJust $ Seq.elemIndexL destination circle
        (firstCirclePart, secondCirclePart) = Seq.splitAt (destinationIndex+1) circle

formatGoalState :: Seq Int -> String
formatGoalState (1 :<| rest) = map intToDigit $ toList rest
formatGoalState (n :<| rest) = formatGoalState $ rest |> n

part1slow = do
  let allStates = iterate playGame $ fromList input
  let goalState = allStates !! 100
  print $ formatGoalState goalState

findDestination :: Int -> [Int] -> Int -> Int
findDestination max pickedup 0 = findDestination max pickedup max
findDestination max pickedup n = case n `elem` pickedup of
  -- The index was not in the picked up set and must therefore be present in the circle
  False -> n
  -- The index was in the picked up set, continue on with the next lower destination
  True  -> findDestination max pickedup (n-1)

playGameMutable :: Int -> V.MVector RealWorld Int -> Int -> IO Int -- vector modified in place :O
playGameMutable numCards cups currentIdx = do
  next3fstIdx <- V.read cups currentIdx
  next3midIdx <- V.read cups next3fstIdx
  next3endIdx <- V.read cups next3midIdx
  circleStartIdx <- V.read cups next3endIdx
  let newCurrent = circleStartIdx
  let pickedup = [next3fstIdx, next3midIdx, next3endIdx] -- dit moet juist de values zijn
  let destination = findDestination numCards pickedup (currentIdx - 1)
  afterDestination <- V.read cups destination
  -- current points to circleStart to represent the pickedup cups skip and the pickedup cups
  -- get spliced into destination
  V.write cups currentIdx circleStartIdx
  V.write cups destination next3fstIdx
  V.write cups next3endIdx afterDestination
  return newCurrent

constructLinkedVector :: [Int] -> IO (Vec.MVector RealWorld Int)
constructLinkedVector input = do
  let inputs = Vec.fromList $ map snd $ sortBy (comparing fst) $ (0,0) : (last input, head input) : zip input (tail input)
  Vec.thaw inputs

findNvaluesAfterX :: Vec.Vector Int -> Int -> Int -> [Int]
findNvaluesAfterX v x 0 = []
findNvaluesAfterX v x n = (v ! x) : findNvaluesAfterX v (v ! x) (n-1)

runGameNtimes numCards inputVector current 0 = return ()
runGameNtimes numCards inputVector current n = do
  newCurrent <- playGameMutable numCards inputVector current
  runGameNtimes numCards inputVector newCurrent (n-1)

part1 = do
  vectorInput <- constructLinkedVector input
  runGameNtimes 9 vectorInput (head input) 100
  result <- Vec.freeze vectorInput
  print $ findNvaluesAfterX result 1 8

part2 = do
  vectorInput <- constructLinkedVector (input ++ [10..1000000])
  runGameNtimes 1000000 vectorInput (head input) 10000000
  result <- Vec.freeze vectorInput
  print $ findNvaluesAfterX result 1 2
