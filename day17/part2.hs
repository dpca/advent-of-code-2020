import qualified Data.Set as S

type Coordinate = (Int, Int, Int, Int)

parseInput :: String -> S.Set Coordinate
parseInput input = foldl insertY S.empty $ zip [0..] (lines input)
    where insertY m     (y, line) = foldl (insertX y) m $ zip [0..] line
          insertX y acc (x, val)  = if val == '#'
                                       then S.insert (x, y, 0, 0) acc
                                       else acc

surroundingSpaces :: [Coordinate]
surroundingSpaces =
    let m = concatMap (\w -> concatMap (\z -> concatMap (\y -> map (\x -> (x, y, z, w)) [-1..1]) [-1..1]) [-1..1]) [-1..1]
     in filter (\(x, y, z, w) -> (x, y, z, w) /= (0, 0, 0, 0)) m

countSurrounding :: S.Set Coordinate -> Coordinate -> Int
countSurrounding input (x, y, z, w) =
    let move (xs, ys, zs, ws) = (x + xs, y + ys, z + zs, w + ws)
     in length $ filter (`S.member` input) $ map move surroundingSpaces

checkSpace :: S.Set Coordinate -> [Coordinate]
checkSpace input =
    let xSpace = S.map (\(x, _, _, _) -> x) input
        ySpace = S.map (\(_, y, _, _) -> y) input
        zSpace = S.map (\(_, _, z, _) -> z) input
        wSpace = S.map (\(_, _, _, w) -> w) input
     in concatMap (\w -> concatMap (\z -> concatMap (\y -> map (\x -> (x, y, z, w)) [minimum xSpace - 1..maximum xSpace + 1]) [minimum ySpace - 1..maximum ySpace + 1]) [minimum zSpace - 1..maximum zSpace + 1]) [minimum wSpace - 1..maximum wSpace + 1]

alterSpace :: S.Set Coordinate -> [Coordinate] -> S.Set Coordinate
alterSpace input = foldl innerFold S.empty
    where innerFold acc space
            | S.member space input = if countSurrounding input space `elem` [2, 3]
                                        then S.insert space acc
                                        else acc
            | otherwise = if countSurrounding input space == 3
                             then S.insert space acc
                             else acc

runCycle 0 input = input
runCycle num input = runCycle (num - 1) $ alterSpace input (checkSpace input)

main = do
    input <- readFile "input.txt"
    let activeInputs = parseInput input
    print $ length $ runCycle 6 activeInputs
