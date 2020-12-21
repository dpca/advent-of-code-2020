import Text.Parsec
import Text.Parsec.String
import Data.List
import Data.List.Split

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
data Tile = Tile {
                    num :: Int,
                    pic :: [String],
                    leftMatches :: [Matches],
                    rightMatches :: [Matches],
                    topMatches :: [Matches],
                    botMatches :: [Matches]
                 } deriving (Show)

tileTop :: Tile -> String
tileTop Tile { pic = d } = head d

tileLeft :: Tile -> String
tileLeft Tile { pic = d } = foldl (\acc x -> acc ++ [head x]) [] d

tileBot :: Tile -> String
tileBot Tile { pic = d } = last d

tileRight :: Tile -> String
tileRight Tile { pic = d } = foldl (\acc x -> acc ++ [last x]) [] d

parseTile :: [String] -> Tile
parseTile (num:lst) = Tile { num = parseIt tileNum num, pic = lst, topMatches = [], botMatches = [], leftMatches = [], rightMatches = [] }

rotateOnce :: Tile -> Tile
rotateOnce tile@Tile { pic = d } = tile { pic = map reverse $ transpose d }

rotateTile :: Int -> Tile -> Tile
rotateTile 0 tile = tile
rotateTile i tile = rotateTile (i - 1) (rotateOnce tile)

flipTile :: Tile -> Tile
flipTile tile@Tile { pic = d } = tile { pic = reverse d }

findMatches :: Tile -> [Tile] -> Tile
findMatches tile [] = tile
findMatches tile@Tile { num = n } (Tile { num = n2 }:xs) | n == n2 = findMatches tile xs
findMatches tile@Tile { num = n, pic = p, leftMatches = l, rightMatches = r, topMatches = u, botMatches = d } (tile2@Tile { num = n2 }:xs) =
    let rotateIt t   = map (\n -> (n, rotateTile n t)) [0..3]
        allRotations = concatMap rotateIt [tile2, flipTile tile2]
        toMatches    = map (\(n, _) -> TileRotation { rotation = n, tile = n2 })
        bMatches     = toMatches $ filter (\(_, t) -> tileBot tile == tileTop t) allRotations
        tMatches     = toMatches $ filter (\(_, t) -> tileTop tile == tileBot t) allRotations
        lMatches     = toMatches $ filter (\(_, t) -> tileLeft tile == tileRight t) allRotations
        rMatches     = toMatches $ filter (\(_, t) -> tileRight tile == tileLeft t) allRotations
        newTile      = Tile { num = n,
                              pic = p,
                              leftMatches = l ++ lMatches,
                              rightMatches = r ++ rMatches,
                              topMatches = u ++ tMatches,
                              botMatches = d ++ bMatches }
     in findMatches newTile xs

findCorners :: [Tile] -> [Tile]
findCorners = go []
    where go acc [] = acc
          go acc (tile@Tile { leftMatches = l, rightMatches = r, topMatches = t, botMatches = b }:xs) =
              if length (filter (not . null) [l, r, t, b]) == 2 then go (tile:acc) xs else go acc xs

main = do
    input <- readFile "input.txt"
    let tiles = map (parseTile . lines) $ splitOn "\n\n" input
    let matchedTiles = map (`findMatches` tiles) tiles
    print $ product $ map (\Tile {num = n} -> n) $ findCorners matchedTiles
