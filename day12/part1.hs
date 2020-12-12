data Instruction v = North v | South v | East v | West v | Left v | Right v | Forward v

parseInput :: IO [String]
parseInput = do
    input <- readFile "input.txt"
    return $ lines input

type X = Int
type Y = Int
type Bearing = Int

execute :: (X, Y, Bearing) -> String -> (X, Y, Bearing)
execute (x, y, bearing) (instruction:lst) =
    let val = read lst
        alterBearing newBearing
          | newBearing < 0 = newBearing + 360
          | newBearing >= 360 = newBearing - 360
          | otherwise = newBearing
        driveForward
          | bearing == 0 = (x, y + val, bearing)
          | bearing == 90 = (x + val, y, bearing)
          | bearing == 180 = (x, y - val, bearing)
          | bearing == 270 = (x - val, y, bearing)
     in case instruction of
          'N' -> (x, y + val, bearing)
          'S' -> (x, y - val, bearing)
          'E' -> (x + val, y, bearing)
          'W' -> (x - val, y, bearing)
          'L' -> (x, y, alterBearing $ bearing - val)
          'R' -> (x, y, alterBearing $ bearing + val)
          'F' -> driveForward

manhattanDistance :: (X, Y) -> (X, Y) -> Int
manhattanDistance (initX, initY) (finalX, finalY) = abs (finalX - initX) + abs (finalY - initY)

main = do
    input <- parseInput
    let (x, y, _) = foldl execute (0, 0, 90) input
    print $ manhattanDistance (0, 0) (x, y)
