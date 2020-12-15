import Data.IntMap

numberAt :: [Int] -> Int -> Int
numberAt startingNumbers end = go 1 (head startingNumbers) empty
    where startLen = length startingNumbers
          go idx prevNum prevMap
            | idx == end             = prevNum
            | idx < startLen         = go nextIdx (startingNumbers !! idx) newMap
            | member prevNum prevMap = go nextIdx (idx - (prevMap ! prevNum)) newMap
            | otherwise              = go nextIdx 0 newMap
            where nextIdx = idx + 1
                  newMap = insert prevNum idx prevMap

main = do
    let input = [1,0,16,5,17,4]
    print $ "Part 1: " ++ show (numberAt input 2020)
    print $ "Part 2: " ++ show (numberAt input 30000000)
