import Text.Parsec
import Text.Parsec.String
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

data Tile = Black | White deriving (Eq)

parseIt :: Parser a -> String -> a
parseIt p x = case parse p "" x of
                Left  err -> error (show err)
                Right res -> res

allDirections :: [String]
allDirections = ["e", "se", "sw", "w", "nw", "ne"]

goDirection :: (Int, Int, Int) -> String -> (Int, Int, Int)
goDirection (x,y,z) dir =
    case dir of
      "e"  -> (x + 1, y + 1, z    )
      "se" -> (x,     y + 1, z + 1)
      "sw" -> (x - 1, y,     z + 1)
      "w"  -> (x - 1, y - 1, z    )
      "nw" -> (x,     y - 1, z - 1)
      "ne" -> (x + 1, y,     z - 1)

parseDirections :: Parser [String]
parseDirections = many1 $ foldl1 (\a s -> try a <|> try s) $ map string allDirections

processDirections :: [String] -> (Int, Int, Int)
processDirections = go (0, 0, 0)
    where go acc [] = acc
          go coords (dir:lst) = go (goDirection coords dir) lst

createMap :: [[String]] -> M.Map (Int, Int, Int) Tile
createMap = foldl innerFold M.empty
    where innerFold acc dir = M.insertWith (\_ old -> if old == Black then White else Black) (processDirections dir) Black acc

adjacentTiles :: (Int, Int, Int) -> [(Int, Int, Int)]
adjacentTiles coords = map (goDirection coords) allDirections

lookupTile :: M.Map (Int, Int, Int) Tile -> (Int, Int, Int) -> Tile
lookupTile acc k = fromMaybe White $ M.lookup k acc

numAdjacent :: M.Map (Int, Int, Int) Tile -> (Int, Int, Int) -> Int
numAdjacent acc coords = length $ filter (\dir -> lookupTile acc (goDirection coords dir) == Black) allDirections

runDay :: Int -> M.Map (Int, Int, Int) Tile -> M.Map (Int, Int, Int) Tile
runDay 0 grid = grid
runDay n grid =
    let trueGrid = M.filter (== Black) grid
        allCoords = M.foldrWithKey (\k _ acc -> foldr S.insert acc (k : adjacentTiles k)) S.empty trueGrid
        innerFold coords acc =
            case lookupTile grid coords of
              Black -> if numAdjacent grid coords == 0 || numAdjacent grid coords > 2
                         then M.insert coords White acc
                         else acc
              White -> if numAdjacent grid coords == 2
                          then M.insert coords Black acc
                          else acc
     in runDay (n - 1) $ S.foldr innerFold grid allCoords

main = do
    input <- readFile "input.txt"
    let directions = map (parseIt parseDirections) (lines input)
    let grid = createMap directions
    print $ "Part 1: " ++ show (length $ filter (\(_, v) -> v == Black) $ M.toList grid)

    let part2 = runDay 100 grid
    print $ "Part 2: " ++ show (length $ filter (\(_, v) -> v == Black) $ M.toList part2)
