import Data.List

main = do
    contents <- readFile "input.txt"
    let lst = toInt $ words contents

    let (x, y) = findSum lst lst
    print $ "x = " ++ show x
    print $ "y = " ++ show y
    print $ "Multiplied = " ++ show (x * y)

    let (x, y, z) = findSum3 lst lst lst
    print $ "x = " ++ show x
    print $ "y = " ++ show y
    print $ "z = " ++ show z
    print $ "Multiplied = " ++ show (x * y * z)


toInt :: [String] -> [Int]
toInt = map read

findSum :: [Int] -> [Int] -> (Int, Int)
findSum (x:xs) lst =
    case find (\y -> x + y == 2020) lst of
      Nothing -> findSum xs lst
      Just y  -> (x, y)

findSum3 :: [Int] -> [Int] -> [Int] -> (Int, Int, Int)
findSum3 [] (_:ys) lst = findSum3 lst ys lst
findSum3 (x:xs) (y:ys) lst =
    case find (\z -> x + y + z == 2020) lst of
      Nothing -> findSum3 xs (y:ys) lst
      Just z  -> (x, y, z)
