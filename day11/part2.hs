getInput :: IO [String]
getInput = do
    input <- readFile "input.txt"
    return $ lines input

type SeatMap = [[Char]]
type Dir = Int
type XLoc = Int
type YLoc = Int
type Width = Int
type Height = Int

viewDirection :: Dir -> Dir -> SeatMap -> Width -> Height -> XLoc -> YLoc -> Int
viewDirection xDir yDir seatMap w h x y = seat
    where xLoc = x + xDir
          yLoc = y + yDir
          seatHere = seatMap !! yLoc !! xLoc
          seat
            | xLoc < 0 = 0
            | xLoc > w = 0
            | yLoc < 0 = 0
            | yLoc > h = 0
            | seatHere == 'L' = 0
            | seatHere == '#' = 1
            | otherwise = viewDirection xDir yDir seatMap w h xLoc yLoc

viewNW = viewDirection (-1) (-1)
viewN  = viewDirection 0 (-1)
viewNE = viewDirection 1 (-1)
viewE  = viewDirection 1 0
viewSE = viewDirection 1 1
viewS  = viewDirection 0 1
viewSW = viewDirection (-1) 1
viewW  = viewDirection (-1) 0

allViews = [viewNW, viewN, viewNE, viewE, viewSE, viewS, viewSW, viewW]

runModel :: SeatMap -> SeatMap
runModel seatMap = newSeatMap
    where height = length seatMap - 1
          width  = length (head seatMap) - 1
          newSeatMap = map (\y -> map (`makeChange` y) [0..width]) [0..height]
          makeChange x y = case seatMap !! y !! x of
                             '.' -> '.'
                             'L' -> if seenSeats x y == 0 then '#' else 'L'
                             '#' -> if seenSeats x y <= 4 then '#' else 'L'
          seenSeats x y = sum $ map (\v -> v seatMap width height x y) allViews

iterateModel :: SeatMap -> SeatMap
iterateModel seatMap = run seatMap $ runModel seatMap
    where run lastMap thisMap
            | lastMap == thisMap = thisMap
            | otherwise = run thisMap $ runModel thisMap

main = do
    lines <- getInput
    let finalModel = iterateModel lines
    print $ length $ concatMap (filter (== '#')) finalModel
