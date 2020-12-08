import Utils
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Vector ((!), (!?))
import Debug.Trace

data Instruction = Acc Int | Jmp Int | Nop Int deriving (Show,Eq)

acc :: Parser Instruction
acc = do
  string "acc "
  val <- integer
  return $ Acc val

jmp :: Parser Instruction
jmp = do
  string "jmp "
  val <- integer
  return $ Jmp val

nop :: Parser Instruction
nop = do
  string "nop "
  val <- integer
  return $ Nop val

instruction :: Parser Instruction
instruction = do
  choice [acc,jmp,nop]

step :: (V.Vector Instruction, Int, Int) -> (V.Vector Instruction, Int, Int)
step (program, ip, acc) = case program !? ip of
  Nothing        -> (program, ip, acc)
  Just (Nop _)   -> (program, ip+1, acc)
  Just (Jmp val) -> (program, ip+val, acc)
  Just (Acc val) -> (program, ip+1, acc+val)

firstSeenTwice :: Ord a => S.Set a -> [a] -> a
firstSeenTwice s [] = error "no duplicates found"
firstSeenTwice s (x:xs)
  | S.member x s = x
  | otherwise    = firstSeenTwice (S.insert x s) xs

part1 = do
  Right opcodes <- parseFileLines instruction "day8_input.txt"
  let program = V.fromList opcodes
  let allStates = iterate step (program,0,0)
  let firstRepeatedInstruction = firstSeenTwice S.empty $ map (\(_,x,_) -> x) allStates
  print . (\(x,y,z) -> z) . last . takeWhile (\(_,x,_) -> x /= firstRepeatedInstruction) . tail $ dropWhile (\(_,x,_) -> x /= firstRepeatedInstruction) allStates

runUntilLooping :: V.Vector Instruction -> Maybe Int
runUntilLooping program = if firstRepeatedInstruction >= programSize then Just accAtFirstRepeatedInstruction else Nothing
  where allStates = iterate step (program,0,0)
        firstRepeatedInstruction = firstSeenTwice S.empty $ map (\(_,x,_) -> x) allStates
        programSize = V.length program
        accAtFirstRepeatedInstruction = (\(x,y,z) -> z) . head $ dropWhile (\(_,x,_) -> x /= firstRepeatedInstruction) allStates

isNop :: Instruction -> Bool
isNop (Nop _) = True
isNop _       = False
isJmp :: Instruction -> Bool
isJmp (Jmp _) = True
isJmp _       = False

nopToJmp :: Instruction -> Instruction
nopToJmp (Nop x) = Jmp x
nopToJmp _ = error "non-nop converted to jmp"
jmpToNop :: Instruction -> Instruction
jmpToNop (Jmp x) = Nop x
jmpToNop _ = error "non-jmp converted to nop"

part2 = do
  Right opcodes <- parseFileLines instruction "day8_input.txt"
  let program = V.fromList opcodes
  let nopToJmps = [ program V.// [(x,nopToJmp $ program ! x)] | x <- [0..((V.length program) - 1)], isNop $ program ! x ]
  let jmpToNops = [ program V.// [(x,jmpToNop $ program ! x)] | x <- [0..((V.length program) - 1)], isJmp $ program ! x ]
  let allAlternativePrograms = jmpToNops ++ nopToJmps
  print . head . filter isJust $ map runUntilLooping allAlternativePrograms
