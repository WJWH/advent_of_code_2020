import Utils
import Data.Foldable
import Data.Sequence as Seq (Seq( Empty, (:<|)), (|>), fromList, length, take)
import qualified Data.Set as S

playerDecks :: Parser (Seq Int, Seq Int)
playerDecks = do
  string "Player 1:\n"
  p1 <- integer `endBy` endOfLine
  endOfLine
  string "Player 2:\n"
  p2 <- integer `sepBy` endOfLine
  return $ (Seq.fromList p1, Seq.fromList p2)

playGame :: Seq Int -> Seq Int -> Seq Int
playGame Seq.Empty p2 = p2
playGame p1 Seq.Empty = p1
playGame (c1 :<| p1) (c2 :<| p2) = if c1 > c2
  then playGame (p1 |> c1 |> c2) p2
  else playGame p1 (p2 |> c2 |> c1)

part1 = do
  Right decks <- parseFile playerDecks "day22_input.txt"
  let gameResult = playGame (fst decks) (snd decks)
  print . sum . zipWith (*) [1..] . reverse $ toList gameResult

data Winner = Player1 | Player2 deriving (Show,Eq)

playRecursiveGame :: S.Set (Seq Int,Seq Int) -> Seq Int -> Seq Int -> (Winner,Seq Int)
playRecursiveGame _ Seq.Empty p2 = (Player2,p2)
playRecursiveGame _ p1 Seq.Empty = (Player1,p1)
playRecursiveGame prevs d1@(c1 :<| p1) d2@(c2 :<| p2)
  -- if state has already been seen, player 1 wins
  | S.member (d1,d2) prevs = (Player1,d1)
  -- Both players have enough cards to recurse
  | (c1 <= Seq.length p1) && (c2 <= Seq.length p2) =
    if Player1 == (fst $ playRecursiveGame S.empty (Seq.take c1 p1) (Seq.take c2 p2))
      then playRecursiveGame (S.insert (d1,d2) prevs) (p1 |> c1 |> c2) p2
      else playRecursiveGame (S.insert (d1,d2) prevs) p1 (p2 |> c2 |> c1)
  -- if at least one player doesn't have enough cards to recurse,
  -- the winner of the round is the player with the higher value card
  -- (don't forget to update the set with the state at the start of the round)
  | otherwise = if c1 > c2
    then playRecursiveGame (S.insert (d1,d2) prevs) (p1 |> c1 |> c2) p2
    else playRecursiveGame (S.insert (d1,d2) prevs) p1 (p2 |> c2 |> c1)

part2 = do
  Right decks <- parseFile playerDecks "day22_input.txt"
  let gameResult = playRecursiveGame S.empty (fst decks) (snd decks)
  print . sum . zipWith (*) [1..] . reverse . toList $ snd gameResult
