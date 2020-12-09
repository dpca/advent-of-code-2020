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

findContiguous :: [Int] -> [Int] -> Int -> [Int]
findContiguous (x:xs) [] target = findContiguous xs [x] target
findContiguous lst@(x:xs) prev@(_:ys) target
  | x + s == target = x:prev
  | x + s > target  = findContiguous lst ys target
  | otherwise       = findContiguous xs (prev ++ [x]) target
    where s = sum prev

main = do
    lines <- getInput
    let (preamble, rest) = splitAt 25 lines
    let target = findFirstInvalid preamble rest
    print target
    let contiguous = findContiguous lines [] target
    let l = minimum contiguous
    let m = maximum contiguous
    print $ l + m
