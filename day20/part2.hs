import Text.Parsec
import Text.Parsec.String
import Data.List
import Data.List.Split
import qualified Data.Map as M

parseIt :: Parser a -> String -> a
parseIt p x = case parse p "" x of
                Left  err -> error (show err)
                Right res -> res

tileNum :: Parser Int
tileNum = do
    string "Tile "
    num <- many1 digit
    char ':'
    return (read num)

data Matches = TileRotation { rotation :: Int, tile :: Int } deriving (Show)
data Tile = Tile { num :: Int, pic :: [String] } deriving (Show)

tileTop :: Tile -> String
tileTop Tile { pic = d } = head d

tileLeft :: Tile -> String
tileLeft Tile { pic = d } = foldl (\acc x -> acc ++ [head x]) [] d

tileBot :: Tile -> String
tileBot Tile { pic = d } = last d

tileRight :: Tile -> String
tileRight Tile { pic = d } = foldl (\acc x -> acc ++ [last x]) [] d

parseTile :: [String] -> Tile
parseTile (num:lst) = Tile { num = parseIt tileNum num, pic = lst }

rotatePic :: Int -> [String] -> [String]
rotatePic 0 pic = pic
rotatePic i pic = rotatePic (i - 1) $ map reverse $ transpose pic

rotateTile :: Int -> Tile -> Tile
rotateTile i tile@Tile { pic = d } = tile { pic = rotatePic i d }

flipTile :: Tile -> Tile
flipTile tile@Tile { pic = d } = tile { pic = reverse d }

allRotations :: Tile -> [Tile]
allRotations tile = concatMap rotateIt [tile, flipTile tile]
    where rotateIt tile = map (`rotateTile` tile) [0..3]

findTileBelow :: Tile -> [Tile] -> Maybe Tile
findTileBelow tile = find (\t -> tileBot tile == tileTop t)

findTileAbove :: Tile -> [Tile] -> Maybe Tile
findTileAbove tile = find (\t -> tileTop tile == tileBot t)

findTileRight :: Tile -> [Tile] -> Maybe Tile
findTileRight tile = find (\t -> tileRight tile == tileLeft t)

findTileLeft :: Tile -> [Tile] -> Maybe Tile
findTileLeft tile = find (\t -> tileLeft tile == tileRight t)

createGrid :: [Tile] -> M.Map (Int, Int) Tile
createGrid (t1:tlst) = go (0, 0, t1) (M.fromList [((0, 0), t1)]) tlst
    where go _ acc [] = acc
          go (x, y, t) acc tiles =
              let accTiles = map num $ M.elems acc
                  remainingTiles = filter (\tile -> num tile `notElem` accTiles) tiles
                  allTiles     = concatMap allRotations remainingTiles
                  topMatch     = findTileAbove t allTiles
                  botMatch     = findTileBelow t allTiles
                  rightMatch   = findTileRight t allTiles
                  leftMatch    = findTileLeft t allTiles
                  innerFold a (x, y, m) =
                       case m of
                         Just tile -> go (x, y, tile) (M.insert (x, y) tile a) remainingTiles
                         Nothing -> a
               in foldl innerFold acc [(x, y - 1, topMatch), (x, y + 1, botMatch), (x + 1, y, rightMatch), (x - 1, y, leftMatch)]

gridMapToArray :: M.Map (Int, Int) Tile -> [[Tile]]
gridMapToArray grid =
    let keys = M.keys grid
        minX = minimum $ map fst keys
        maxX = maximum $ map fst keys
        minY = minimum $ map snd keys
        maxY = maximum $ map snd keys
     in map (\y -> map (\x -> grid M.! (x, y)) [minX..maxX]) [minY..maxY]

picWithoutBorder :: [String] -> [String]
picWithoutBorder pic = map dropFirstAndLast (dropFirstAndLast pic)
    where dropFirstAndLast = tail . init

toImage :: [[Tile]] -> [String]
toImage grid =
    let pics = map (map (picWithoutBorder . pic)) grid
        compact = foldl1 (zipWith (++))
     in concatMap compact pics

monster :: [String]
monster = [
            "..................#.",
            "#....##....##....###",
            ".#..#..#..#..#..#..."
          ]

monsterMatch :: [Char] -> [Char] -> Bool
monsterMatch = go
    where go [] _ = True
          go (x:xs) (y:ys) = (x == '.' || x == y) && go xs ys

foundMonsters :: ([Char], [Char], [Char]) -> Int
foundMonsters = go 0
    where go n (s1, _, _) | length s1 < length (head monster) = n
          go n (s1@(_:xs1), s2@(_:xs2), s3@(_:xs3)) =
              if allStringsMatch then go (n+1) (xs1, xs2, xs3) else go n (xs1, xs2, xs3)
                  where allStringsMatch = firstMatch && secondMatch && thirdMatch
                        firstMatch  = monsterMatch (head monster) s1
                        secondMatch = monsterMatch (monster !! 1) s2
                        thirdMatch  = monsterMatch (monster !! 2) s3

findMonsters :: [String] -> Int
findMonsters = go 0
    where go num [x,y,z] = num + foundMonsters (x,y,z)
          go num (x:y:z:lst) = num + foundMonsters (x,y,z) + findMonsters (y:z:lst)

allGrids :: [String] -> [[String]]
allGrids grid = concatMap rotateIt [grid, reverse grid]
    where rotateIt g = map (`rotatePic` g) [0..3]

main = do
    input <- readFile "input.txt"
    let tiles = map (parseTile . lines) $ splitOn "\n\n" input
    let grid = gridMapToArray $ createGrid tiles
    let gridImage = toImage grid
    let checkGrids = allGrids gridImage
    let monsters = find (/= 0) $ map findMonsters checkGrids
    let waterRoughness = length $ concatMap (filter (== '#')) gridImage
    case monsters of
      Just x  -> print $ waterRoughness - (x * 15)
      Nothing -> error "Didn't find any monsters"
