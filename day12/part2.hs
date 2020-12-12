data Instruction v = North v | South v | East v | West v | Left v | Right v | Forward v

parseInput :: IO [String]
parseInput = do
    input <- readFile "input.txt"
    return $ lines input

type X = Int
type Y = Int

execute :: (X, Y, X, Y) -> String -> (X, Y, X, Y)
execute (shipX, shipY, waypointX, waypointY) (instruction:lst) =
    let val = read lst
        rotateWaypointLeft
          | val == 0 = (shipX, shipY, waypointX, waypointY)
          | val == 90 = (shipX, shipY, -waypointY, waypointX)
          | val == 180 = (shipX, shipY, -waypointX, -waypointY)
          | val == 270 = (shipX, shipY, waypointY, -waypointX)
        rotateWaypointRight
          | val == 0 = (shipX, shipY, waypointX, waypointY)
          | val == 90 = (shipX, shipY, waypointY, -waypointX)
          | val == 180 = (shipX, shipY, -waypointX, -waypointY)
          | val == 270 = (shipX, shipY, -waypointY, waypointX)
        driveForward = (shipX + val * waypointX, shipY + val * waypointY, waypointX, waypointY)
     in case instruction of
          'N' -> (shipX, shipY, waypointX, waypointY + val)
          'S' -> (shipX, shipY, waypointX, waypointY - val)
          'E' -> (shipX, shipY, waypointX + val, waypointY)
          'W' -> (shipX, shipY, waypointX - val, waypointY)
          'L' -> rotateWaypointLeft
          'R' -> rotateWaypointRight
          'F' -> driveForward

manhattanDistance :: (X, Y) -> (X, Y) -> Int
manhattanDistance (initX, initY) (finalX, finalY) = abs (finalX - initX) + abs (finalY - initY)

main = do
    input <- parseInput
    let (x, y, _, _) = foldl execute (0, 0, 10, 1) input
    print $ manhattanDistance (0, 0) (x, y)
