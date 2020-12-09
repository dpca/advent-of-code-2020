getInput :: IO [Int]
getInput = do
    input <- readFile "input.txt"
    return $ map read (lines input)

isValid :: [Int] -> Int -> Bool
isValid preamble num = any result
    where result = [num | (x, xpos) <- zip preamble [0..], (y, ypos) <- zip preamble [0..], xpos /= ypos && x + y == num]
          any [] = False
          any (_:_) = True

findFirstInvalid :: [Int] -> [Int] -> Int
findFirstInvalid preamble@(_:xs) (num:rest)
  | not (isValid preamble num) = num
  | otherwise = findFirstInvalid (xs ++ [num]) rest

main = do
    lines <- getInput
    let (preamble, rest) = splitAt 25 lines
    print $ findFirstInvalid preamble rest
