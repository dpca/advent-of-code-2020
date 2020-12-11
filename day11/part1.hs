getInput :: IO [String]
getInput = do
    input <- readFile "input.txt"
    return $ lines input

type SeatMap = [[Char]]

runModel :: SeatMap -> SeatMap
runModel seatMap = newSeatMap
    where height = length seatMap - 1
          width  = length (head seatMap) - 1
          newSeatMap = map (\y -> map (`makeChange` y) [0..width]) [0..height]
          makeChange x y = case seatMap !! y !! x of
                             '.' -> '.'
                             'L' -> if nearBy x y == 0 then '#' else 'L'
                             '#' -> if nearBy x y <= 4 then '#' else 'L'
          nearBy x y = sum $ concatMap (\y -> map (`lookup` y) [x - 1..x + 1]) [y - 1..y + 1]
          lookup x y
            | x < 0 = 0
            | x > width = 0
            | y < 0 = 0
            | y > height = 0
            | otherwise = if seatMap !! y !! x == '#' then 1 else 0

iterateModel :: SeatMap -> SeatMap
iterateModel seatMap = run seatMap $ runModel seatMap
    where run lastMap thisMap
            | lastMap == thisMap = thisMap
            | otherwise = run thisMap $ runModel thisMap

main = do
    lines <- getInput
    let finalModel = iterateModel lines
    print $ length $ concatMap (filter (== '#')) finalModel
