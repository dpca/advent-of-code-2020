import Data.List.Split
import qualified Data.Set as S

parsePlayer :: String -> [Int]
parsePlayer player =
    let (_:xs) = lines player
     in map read xs

playGame :: [Int] -> [Int] -> Either [Int] [Int]
playGame = go S.empty
    where go _   x [] = Left x
          go _   [] y = Right y
          go acc x y | S.member (x, y) acc = Left x
          go acc x y | S.member (x, y) acc = Left x
          go acc xl@(x:xs) yl@(y:ys) | x > length xs || y > length ys =
              if x > y then go (S.insert (xl, yl) acc) (xs ++ [x,y]) ys
                       else go (S.insert (xl, yl) acc) xs (ys ++ [y,x])
          go acc xl@(x:xs) yl@(y:ys) =
              case playGame (take x xs) (take y ys) of
                Left _  -> go (S.insert (xl, yl) acc) (xs ++ [x,y]) ys
                Right _ -> go (S.insert (xl, yl) acc) xs (ys ++ [y,x])

score :: [Int] -> Int
score lst = go $ zip (reverse lst) [1..]
    where go = foldl (\acc (x, n) -> acc + x * n) 0

main = do
    input <- readFile "input.txt"
    let (player1:player2:_) = map parsePlayer $ splitOn "\n\n" input
    case playGame player1 player2 of
      Left x  -> print $ "Player 1 wins with " ++ show (score x) ++ " points"
      Right y -> print $ "Player 2 wins with " ++ show (score y) ++ " points"
