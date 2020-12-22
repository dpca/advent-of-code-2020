import Data.List.Split

parsePlayer :: String -> [Int]
parsePlayer player =
    let (_:xs) = lines player
     in map read xs

playGame :: [Int] -> [Int] -> Either [Int] [Int]
playGame [] y = Right y
playGame x [] = Left x
playGame (x:xs) (y:ys) =
    if x > y then playGame (xs ++ [x,y]) ys else playGame xs (ys ++ [y,x])

score :: [Int] -> Int
score lst = go $ zip (reverse lst) [1..]
    where go = foldl (\acc (x, n) -> acc + x * n) 0

main = do
    input <- readFile "input.txt"
    let (player1:player2:_) = map parsePlayer $ splitOn "\n\n" input
    case playGame player1 player2 of
      Left x  -> print $ score x
      Right y -> print $ score y
