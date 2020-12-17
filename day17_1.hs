import Utils
import qualified Data.Map as M
import qualified Data.Set as S

data Cube = Inactive | Active deriving (Show,Eq)
type Position = (Int,Int,Int)
type PocketDimension = M.Map Position Cube

neighborsCoords :: Position -> [Position]
neighborsCoords (x,y,z) = filter (/= (x,y,z)) $ (,,) <$> [x-1..x+1] <*> [y-1..y+1] <*> [z-1..z+1]

singleCube :: PocketDimension -> Position -> (Position,Cube)
singleCube pd pos@(x,y,z) = case cube of
  Inactive -> if numActiveNeighbors == 3 then (pos,Active) else (pos,Inactive)
  Active   -> if numActiveNeighbors == 2 || numActiveNeighbors == 3 then (pos,Active) else (pos,Inactive)
  where numActiveNeighbors = sum [ 1 | nc <- neighborsCoords (x,y,z) , M.lookup nc pd == Just Active]
        cube = fromMaybe Inactive $ M.lookup (x,y,z) pd

-- stop and think: only active cubes and inactive cubes next to active cubes can change
-- so why iterate over all the coordinates in some space when you only have to check the
-- subset of active cubes and their neighbors?
-- You can further cut down on the amount of things looked at by deduping the neighbors of
-- active cubes with a Set.
-- Also: only store active cubes, just delete empty ones

ordNub = S.toList . S.fromList

step :: PocketDimension -> PocketDimension
step pd = M.filter (== Active) . M.fromList $ map (singleCube pd) cellsToBeLookedAt
  where actives = M.keys pd
        neighbors = concat $ map neighborsCoords actives
        cellsToBeLookedAt = ordNub $ actives ++ neighbors

part1 = do
  x <- lines <$> readFile "day17_input.txt"
  let lineHeight = length x
  let lineWidth  = length $ head x
  let initialCoords = [ (x,y,0) | y <- [1..lineWidth], x <- [1..lineHeight]] 
  let initialMap = M.fromList . filter (\x -> snd x == Active) $ zip initialCoords (map (\c -> if c == '#' then Active else Inactive) $ concat x)
  let allStates = iterate step initialMap
  print . M.size $ allStates !! 6
