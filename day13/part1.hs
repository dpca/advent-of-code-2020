import Data.List.Split

parseInput :: IO (Int, [String])
parseInput = do
    input <- readFile "input.txt"
    let (earliestTime:busSchedule:_) = lines input
    return (read earliestTime, splitOn "," busSchedule)

type BusNumber = Int
type Departure = Int

firstDeparture :: Int -> BusNumber -> (BusNumber, Departure)
firstDeparture time bus = go 0
    where go depart
            | depart < time = go $ depart + bus
            | otherwise     = (bus, depart)

earliestBus :: Int -> [BusNumber] -> (BusNumber, Departure)
earliestBus time buses = foldl1 go departures
    where departures = map (firstDeparture time) buses
          go old@(_, oldDepart) new@(_, newDepart)
            | newDepart < oldDepart = new
            | otherwise = old

main = do
    (time, schedule) <- parseInput
    let buses = map read $ filter (/= "x") schedule :: [Int]
    let (bus, depart) = earliestBus time buses
    print $ bus * (depart - time)
