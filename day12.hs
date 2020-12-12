import Utils
import Data.List

data Direction = Northward | Eastward | Southward | Westward deriving (Show,Eq,Enum)
data Instruction = North Int | East Int | South Int | West Int | BB Int | SB Int | Forward Int deriving (Show,Eq)

step :: (Direction,Int,Int) -> Instruction -> (Direction,Int,Int)
step (d,x,y) (North n) = (d,x,y+n)
step (d,x,y) (South n) = (d,x,y-n)
step (d,x,y) (East n) = (d,x+n,y)
step (d,x,y) (West n) = (d,x-n,y)
-- using the fact that it's an enum
step (d,x,y) (SB 0) = (d,x,y)
step (Westward,x,y) (SB n) = step (Northward,x,y) (SB (n-90))
step (d,x,y) (SB n) = step (succ d,x,y) (SB (n-90))
step (d,x,y) (BB 0) = (d,x,y)
step (Northward,x,y) (BB n) = step (Westward,x,y) (BB (n-90))
step (d,x,y) (BB n) = step (pred d,x,y) (BB (n-90))
-- Just writing out the combinations, it's only 4 lines
step (Northward,x,y) (Forward n) = (Northward,x,y+n)
step (Southward,x,y) (Forward n) = (Southward,x,y-n)
step (Eastward,x,y)  (Forward n) = (Eastward,x+n,y)
step (Westward,x,y)  (Forward n) = (Westward,x-n,y)

instruction :: Parser Instruction
instruction = do
  action <- anyChar
  distance <- integer
  return $ case action of
    'N' -> North distance
    'S' -> South distance
    'E' -> East distance
    'W' -> West distance
    'L' -> BB distance
    'R' -> SB distance
    'F' -> Forward distance

part1 = do
  Right instructions <- parseFileLines instruction "day12_input.txt"
  print . (\(_,x,y) -> abs x + abs y) $ foldl' step (Eastward,0,0) instructions

stepWithWaypoint :: (Int,Int,Int,Int) -> Instruction -> (Int,Int,Int,Int)
stepWithWaypoint (x,y,a,b) (North n) = (x,y+n,a,b)
stepWithWaypoint (x,y,a,b) (South n) = (x,y-n,a,b)
stepWithWaypoint (x,y,a,b) (East n) = (x+n,y,a,b)
stepWithWaypoint (x,y,a,b) (West n) = (x-n,y,a,b)
-- Rotation matrices, yay! 
stepWithWaypoint (x,y,a,b) (BB  90) = (-1*y,x,a,b)
stepWithWaypoint (x,y,a,b) (BB 180) = (-1*x,-1*y,a,b)
stepWithWaypoint (x,y,a,b) (BB 270) = (y,-1*x,a,b)
stepWithWaypoint (x,y,a,b) (SB n) = stepWithWaypoint (x,y,a,b) (BB (360-n))
stepWithWaypoint (x,y,a,b) (Forward n) = (x,y,a+(n*x),b+(n*y))

part2 = do
  Right instructions <- parseFileLines instruction "day12_input.txt"
  print . (\(_,_,x,y) -> abs x + abs y) $ foldl' stepWithWaypoint (10,1,0,0) instructions
