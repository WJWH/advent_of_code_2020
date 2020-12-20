{-# LANGUAGE TupleSections #-}
import Utils
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

type TileID = Int
type Tile = M.Map (Int,Int) Char
data Rotation = R0 | R90 | R180 | R270 deriving (Show,Eq,Ord)
data Flippation = Flipped | NotFlipped deriving (Show,Eq,Ord)
type Orientation = (Rotation, Flippation)
type OrientedTile = (Orientation,Tile)
type OrientedTileID = (TileID,Orientation)

type MatchSet = S.Set (OrientedTileID, OrientedTileID)

tileRow :: Parser String
tileRow = many (oneOf ".#")

tile :: Parser (TileID,Tile)
tile = do
  string "Tile "
  tileID <- integer
  string ":\n"
  rows <- tileRow `sepBy` endOfLine
  let tile = M.fromList $ zip [(x,y) | y <- [0..9], x <- [0..9]] (concat rows)
  return $ (tileID, tile)

inputFile = do
  tiles <- many tile
  eof
  return tiles

flipTileHorizontally :: Tile -> Tile
-- flipTileHorizontally t =  M.fromList . map (\((x,y),c) -> ((9-x,y),c)) $  M.toList t
flipTileHorizontally t = M.mapKeys (\(x,y) -> ((maxC-x,y))) t
  where maxC = fst . fst . fromJust $ M.lookupMax t

transposeTile :: Tile -> Tile
transposeTile t =  M.fromList . map (\((x,y),c) -> ((y,x),c)) $  M.toList t

rotateTileNinetyDegrees :: Tile -> Tile
rotateTileNinetyDegrees = flipTileHorizontally . transposeTile

tileInAllOrientations :: (TileID, Tile) -> [(TileID, OrientedTile)]
tileInAllOrientations (tid, t) = map (tid,) [orientedOriginal, rot90, rot180, rot270, orientedFlipped, rot90f, rot180f, rot270f]
  where flippedTile = flipTileHorizontally t
        orientedOriginal = ((R0,NotFlipped),t)
        orientedFlipped  = ((R0,Flipped), flippedTile)
        rot90   = ((R90,NotFlipped),rotateTileNinetyDegrees t)
        rot180  = ((R180,NotFlipped),rotateTileNinetyDegrees $ snd rot90)
        rot270  = ((R270,NotFlipped),rotateTileNinetyDegrees $ snd rot180)
        rot90f  = ((R90,Flipped),rotateTileNinetyDegrees flippedTile)
        rot180f = ((R180,Flipped),rotateTileNinetyDegrees $ snd rot90f)
        rot270f = ((R270,Flipped),rotateTileNinetyDegrees $ snd rot180f)

fillHoriSet :: [(TileID,OrientedTile)] -> MatchSet
fillHoriSet ots = S.fromList [ ((fst x,fst $ snd x),(fst y, fst $ snd y)) | x <- ots, y <- ots, isMatch x y ]
  where isMatch :: (TileID,OrientedTile) -> (TileID,OrientedTile) -> Bool
        isMatch (tid1,(o1,t1)) (tid2,(o2,t2))
          | tid1 == tid2 = False -- don't match tiles against themselves
          -- otherwise it's a match iff the bottom row of t1 is the top row of t2
          -- taking advantage of the fact that all tiles are 10x10 here
          | otherwise = all (\row -> t1 M.! (9,row) == t2 M.! (0,row)) [0..9]

fillVertSet :: [(TileID,OrientedTile)] -> MatchSet
fillVertSet ots = S.fromList [ ((fst x,fst $ snd x),(fst y, fst $ snd y)) | x <- ots, y <- ots, isMatch x y ]
  where isMatch :: (TileID,OrientedTile) -> (TileID,OrientedTile) -> Bool
        isMatch (tid1,(o1,t1)) (tid2,(o2,t2))
          | tid1 == tid2 = False -- don't match tiles against themselves
          -- otherwise it's a match iff the rightmost column of t1 is the leftmost column of t2
          -- taking advantage of the fact that all tiles are 10x10 here
          | otherwise = all (\col -> t1 M.! (col,9) == t2 M.! (col,0)) [0..9]

isVerticalMatch :: (TileID,OrientedTile) -> (TileID,OrientedTile) -> Bool
isVerticalMatch (tid1,(o1,t1)) (tid2,(o2,t2))
  | tid1 == tid2 = False -- don't match tiles against themselves
  -- otherwise it's a match iff the rightmost column of t1 is the leftmost column of t2
  -- taking advantage of the fact that all tiles are 10x10 here
  | otherwise = all (\col -> t1 M.! (col,9) == t2 M.! (col,0)) [0..9]

isHorizontalMatch :: (TileID,OrientedTile) -> (TileID,OrientedTile) -> Bool
isHorizontalMatch (tid1,(o1,t1)) (tid2,(o2,t2))
  | tid1 == tid2 = False -- don't match tiles against themselves
  -- otherwise it's a match iff the rightmost column of t1 is the leftmost column of t2
  -- taking advantage of the fact that all tiles are 10x10 here
  | otherwise = all (\row -> t1 M.! (9,row) == t2 M.! (0,row)) [0..9]

ordNub :: Ord a => [a] -> [a]
ordNub = S.toList . S.fromList

-- data Location = TopLeft | TopMid | TopRight | MidLeft | MidMid | MidRight | BottomLeft | BottomMid | BottomRight deriving (Show,Eq)
type Location = (Int,Int)
allLocations :: Int -> [(Int,Int)]
allLocations axisLength = [(x,y) | y <- [0..axisLength-1], x <- [0..axisLength-1]]

bt :: MatchSet -> MatchSet -> [([Location], [OrientedTileID], M.Map Location OrientedTileID)]-> M.Map Location OrientedTileID
bt vs hs []  = error "no solution found"
bt vs hs ((locationsLeft,tilesAvailable,solSoFar):qs) 
  | null locationsLeft = solSoFar
  | otherwise = bt vs hs ((newsols (locationsLeft,tilesAvailable,solSoFar)) ++ qs)
    where newsols :: ([Location], [OrientedTileID], M.Map Location OrientedTileID) -> [([Location], [OrientedTileID], M.Map Location OrientedTileID)]
          newsols ((ll@(x,y):locationsLeft),tilesAvailable,solSoFar) = 
            [ (locationsLeft,filter (\(tid,or) -> tid /= fst nt) tilesAvailable,M.insert ll nt solSoFar) | 
                nt <- tilesAvailable,
                if isJust $ M.lookup (x-1,y) solSoFar then S.member (fromJust (M.lookup (x-1,y) solSoFar), nt) hs else True,
                if isJust $ M.lookup (x,y-1) solSoFar then S.member (fromJust (M.lookup (x,y-1) solSoFar), nt) vs else True
            ]

printTile :: Tile -> IO ()
printTile t = do
  forM_ [0..9] $ \y -> do
    forM_ [0..9] $ \x -> do
      putChar $ (t M.! (x,y))
    putStr "\n"

part1 = do
  Right tiles <- parseFile inputFile "day20_input.txt"
  let allOrientedTiles = concat $ map tileInAllOrientations tiles
  let vertSet = fillVertSet allOrientedTiles
  let horiSet = fillHoriSet allOrientedTiles
  let axisLength = floor . sqrt . fromIntegral $ length tiles
  let both = map (\(tid,(o,t)) -> (tid,o)) allOrientedTiles
  let solution = bt vertSet horiSet [(allLocations axisLength, both, M.empty)]
  print . product . map (\c -> fst $ solution M.! c) $ (,) <$> [0,axisLength-1] <*> [0,axisLength-1]

stitchImages :: M.Map TileID Tile -> M.Map Location OrientedTileID -> M.Map Location Char
stitchImages originalTiles solution = M.foldrWithKey foldfunc M.empty solution 
  where foldfunc :: Location -> OrientedTileID -> M.Map Location Char -> M.Map Location Char
        foldfunc (x,y) (tid,o) acc = foldl' (\tempacc (p,q) -> M.insert (x*8 + (p-1), y*8 + (q-1)) ((orientedOriginal tid o) M.! (p,q)) tempacc) acc locations
        locations = (,) <$> [1..8] <*> [1..8]
        orientedOriginal :: TileID -> Orientation -> Tile
        orientedOriginal tid o = applyOrientation o $ originalTiles M.! tid

applyOrientation :: Orientation -> Tile -> Tile
applyOrientation (R0,NotFlipped) t = t
applyOrientation (R90,NotFlipped)  t = applyOrientation (R0,NotFlipped) $ rotateTileNinetyDegrees t
applyOrientation (R180,NotFlipped) t = applyOrientation (R90,NotFlipped) $ rotateTileNinetyDegrees t
applyOrientation (R270,NotFlipped) t = applyOrientation (R180,NotFlipped) $ rotateTileNinetyDegrees t
applyOrientation (r,Flipped) t = applyOrientation (r,NotFlipped) $ flipTileHorizontally t

printStitchedImage :: Int -> Tile -> IO ()
printStitchedImage axisLength t = do
  forM_ [0..(8*axisLength - 1)] $ \y -> do
    forM_ [0..(8*axisLength - 1)] $ \x -> do
      putChar $ (t M.! (y,x))
    putStr "\n"

-- sea monsters look like this:
--            1111111111
--  01234567890123456789
-- "                  # "
-- "#    ##    ##    ###"
-- " #  #  #  #  #  #   "
-- it is 3 tiles high and 20 tiles wide so we can only check until maxC -2 in y dimension and maxC -19 in x dimension
-- (starting from top left) (images are square so max X == max Y)
findMonsters :: M.Map Location Char -> Int
findMonsters im = sum [ 1 | x <- [0..maxC-19], y <- [0..maxC - 2], monsterHere im (x,y)]
  where maxC = fst . fst . fromJust $ M.lookupMax im

monsterHere :: M.Map Location Char -> (Int,Int) -> Bool
monsterHere im (x,y) = all (\coord -> im M.! coord == '#' ) absCoords
  where absCoords = map (\(p,q) -> (x + p, y + q)) relCoords
        -- all in x,y; x to the right and y downwards
        relCoords = [(0,1),(1,2),(4,2),(5,1),(6,1),(7,2),(10,2),(11,1),(12,1),(13,2),(16,2),(17,1),(18,0),(18,1),(19,1)]

mapInAllOrientations :: M.Map Location Char -> [M.Map Location Char]
mapInAllOrientations t = [orientedOriginal, rot90, rot180, rot270, orientedFlipped, rot90f, rot180f, rot270f]
  where flippedTile = flipTileHorizontally t
        orientedOriginal = t
        orientedFlipped  = flippedTile
        rot90   = rotateTileNinetyDegrees t
        rot180  = rotateTileNinetyDegrees rot90
        rot270  = rotateTileNinetyDegrees rot180
        rot90f  = rotateTileNinetyDegrees flippedTile
        rot180f = rotateTileNinetyDegrees rot90f
        rot270f = rotateTileNinetyDegrees rot180f

part2 = do
  Right tiles <- parseFile inputFile "day20_input.txt"
  let allOrientedTiles = concat $ map tileInAllOrientations tiles
  let vertSet = fillVertSet allOrientedTiles
  let horiSet = fillHoriSet allOrientedTiles
  let axisLength = floor . sqrt . fromIntegral $ length tiles
  let both = map (\(tid,(o,t)) -> (tid,o)) allOrientedTiles
  let solution = bt vertSet horiSet [(allLocations axisLength, both, M.empty)]
  let stitchedImage = stitchImages (M.fromList tiles) solution
  let numMonsters = map findMonsters $ mapInAllOrientations stitchedImage
  print numMonsters
  -- 15 #'s in a monster, so the total answer is the amount of #'s in the entire image minus (15*numMonsters)
  -- assuming they don't overlap ofc
  print . (\x -> x - ((maximum numMonsters) * 15)) . length . filter (== '#') $ (M.elems stitchedImage)
