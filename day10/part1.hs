import Data.List

getInput :: IO [Int]
getInput = do
    input <- readFile "input.txt"
    return $ map read (lines input)

main = do
    input <- getInput
    let adapters = sort input
    let (differences, _) = foldl (\(acc, prev) x -> (acc ++ [x - prev], x)) ([], 0) adapters
    let ones = length $ filter (== 1) differences
    let threes = 1 + length (filter (== 3) differences)
    print $ ones * threes
