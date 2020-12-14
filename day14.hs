import Data.Bits
import Data.Foldable
import qualified Data.IntMap as M
import Utils

data BitMask = BitMask { andMask :: Int, orMask :: Int } deriving (Show,Eq)

data Instruction  = NewBitMask { bm :: BitMask }
                  | MemWrite   { location :: Int, value :: Int }
                  deriving (Show,Eq)

data ProgramState = PS { currentBitmask :: BitMask, memmap :: M.IntMap Int } deriving (Show,Eq)
initialPS = PS (BitMask 0 0) M.empty

bitmask :: Parser Instruction
bitmask = do
  string "mask = "
  chars <- many (oneOf "01X")
  -- andMask is used to force stuff to zero, so all X-es become 1
  let andMask = foldl' (\acc c -> (acc * 2) + (if c == '0' then 0 else 1)) 0 chars
  -- orMask is used to force stuff to zero, so all X-es become 1
  let  orMask = foldl' (\acc c -> (acc * 2) + (if c == '1' then 1 else 0)) 0 chars
  return $ NewBitMask (BitMask andMask orMask)

memwrite :: Parser Instruction
memwrite = do
  string "mem["
  address <- integer
  string "] = "
  value <- integer
  return $ MemWrite address value

instruction :: Parser Instruction
instruction = choice [try bitmask, try memwrite] 

applyBitmask :: BitMask -> Int -> Int
applyBitmask (BitMask am om) v = om .|. (am .&. v)

evaluate :: ProgramState -> [Instruction] -> ProgramState
evaluate ps [] = ps
evaluate (PS bm mm) ((NewBitMask bm'):is) = evaluate (PS bm' mm) is
evaluate (PS bm mm) ((MemWrite k v):is) = evaluate (PS bm (M.insert k (applyBitmask bm v) mm)) is

part1 = do
  Right instructions <- parseFileLines instruction "day14_input.txt"
  print . sum . M.elems . memmap $ evaluate initialPS instructions

retrieveFloatingBits :: BitMask -> Int
retrieveFloatingBits (BitMask am or) = xor am or

applyBitmask2 :: BitMask -> Int -> Int
applyBitmask2 (BitMask am or) v = (v .|. am) .&. complement floats
  where floats = retrieveFloatingBits (BitMask am or)

applyFloatingBits :: Int -> Int -> [Int] -> [Int]
applyFloatingBits bitpos      0 values = values
applyFloatingBits bitpos floats values = case floats .&. 1 of
  0 -> applyFloatingBits (bitpos + 1) (shiftR floats 1) $ values
  1 -> applyFloatingBits (bitpos + 1) (shiftR floats 1) $ (map (\v -> setBit v bitpos) values ++ map (\v -> clearBit v bitpos) values)

evaluate2 :: ProgramState -> [Instruction] -> ProgramState
evaluate2 ps [] = ps
evaluate2 (PS bm mm) ((NewBitMask bm'):is) = evaluate2 (PS bm' mm) is
evaluate2 (PS bm mm) ((MemWrite k v):is) = evaluate2 (PS bm (foldl' (\acc key -> M.insert key v acc) mm keys)) is
  where fs = retrieveFloatingBits bm
        keys = applyFloatingBits 0 fs [applyBitmask2 bm k]

part2 = do
  Right instructions <- parseFileLines instruction "day14_input.txt"
  print .sum . M.elems . memmap $ evaluate2 initialPS instructions
