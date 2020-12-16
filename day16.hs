import Utils
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.MaxFlow
import qualified Data.Map as M

data Field = Field { name :: String, ranges :: [Range]} deriving (Show,Eq,Ord)
data Range = Range Int Int deriving (Show,Eq, Ord)
type Ticket = [Int]

range :: Parser Range
range = do
  start <- integer
  char '-'
  stop <- integer
  return $ Range start stop

field :: Parser Field
field = do
  fieldName <- many (letter <|> char ' ')
  string ": "
  rs <- range `sepBy` string " or "
  return $ Field fieldName rs

ticket :: Parser Ticket
ticket = integer `sepBy` char ','

ticketFile :: Parser ([Field],Ticket,[Ticket])
ticketFile = do
  fields <- field `endBy` endOfLine
  endOfLine
  string "your ticket:\n"
  ownTicket <- ticket
  endOfLine
  endOfLine
  string "nearby tickets:\n"
  nearbys <- ticket `sepBy` endOfLine
  eof
  return (fields,ownTicket,nearbys)

contains :: Range -> Int -> Bool
contains (Range start stop) n = n >= start && n <= stop

part1 = do
  Right (fields,ownTicket,nearbys) <- parseFile ticketFile "day16_input.txt"
  let allRanges = concat $ map ranges fields
  let allValues = concat nearbys
  print . sum $ filter (\n -> all (\r -> not $ r `contains` n) allRanges) allValues

-- It's valid iff on all tickets the value at the index conforms to at least one of the ranges
-- allowed by the field 
isValidAddition :: [Ticket] -> Field -> Int -> Bool
isValidAddition ts f i = all (\tv -> any (\r -> r `contains` tv) (ranges f)) ticketValues
  where ticketValues = map (\t -> t !! i) ts

constructBipartiteGraph :: [Ticket] -> [Field] -> [Int] -> Gr String Int
constructBipartiteGraph ts fs is = mkGraph nodes edges
  where nodes = (startStopIndices ++ fieldIndices ++ unknownIndices)
        startStopIndices = [(-1,"start"), (-2,"stop")]
        fieldIndices = zip [100..] (map name fs)
        unknownIndices = zip is $ map show is
        fieldMap = M.fromList $ zip fs [100..]
        edges = concat [startEdges, stopEdges, problemEdges]
        startEdges = [(-1,n,1) | n <- is ]
        stopEdges  = [(n,-2,1) | n <- map fst fieldIndices]
        problemEdges = [(i,fieldMap M.! f,1) | i <- is, f <- fs, isValidAddition ts f i]

part2 = do
  Right (fields,ownTicket,nearbys) <- parseFile ticketFile "day16_input.txt"
  let allRanges = concat $ map ranges fields
  let validTickets = filter (\ns -> all (\n -> any (\r -> r `contains` n) allRanges) ns) nearbys  
  let graph = constructBipartiteGraph validTickets fields [0.. (length fields) - 1]
  let ownTicketIndices = map (\(a,b,c) -> a) . filter (\(a,b,(x,_)) -> x /= 0 && a >= 0 && b >= 100 && b <= 105) . labEdges $ maxFlowgraph graph (-1) (-2)
  print . product $ map (ownTicket !!) ownTicketIndices
