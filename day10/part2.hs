import Data.List
import Data.List.Split

getInput :: IO [Int]
getInput = do
    input <- readFile "input.txt"
    return $ map read (lines input)

consolidateDiffs :: [[Int]] -> [Int]
consolidateDiffs = foldl consolidate []
    where consolidate acc [] = acc
          consolidate acc (3:_) = acc
          consolidate acc lst@(1:_) = acc ++ [length lst]

diffMultiplier 1 = 1
diffMultiplier 2 = 2
diffMultiplier 3 = 4
diffMultiplier 4 = 7
diffMultiplier 5 = 13

main = do
    input <- getInput
    let adapters = sort input
    let (differences, _) = foldl (\(acc, prev) x -> (acc ++ [x - prev], x)) ([], 0) adapters
    let splits = consolidateDiffs $ split (whenElt (== 3)) differences
    print $ product (map diffMultiplier splits)
