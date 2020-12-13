import Data.List.Split
import Data.Bifunctor

parseInput :: IO (Int, [String])
parseInput = do
    input <- readFile "input.txt"
    let (earliestTime:busSchedule:_) = lines input
    return (read earliestTime, splitOn "," busSchedule)

type BusNumber = Int
type Offset = Int
type Bus = (BusNumber, Offset)

parseSchedule :: [String] -> [Bus]
parseSchedule schedule = map (first read) buses
    where buses = filter (\(s, _) -> s /= "x") $ zip schedule [0..]

findTimeStamp :: Int -> Int -> [Bus] -> Int
findTimeStamp timestamp _ [] = timestamp
findTimeStamp timestamp jumpBy buses@((bus,offset):rest)
  | mod (timestamp + offset) bus == 0 = findTimeStamp timestamp (jumpBy * bus) rest
  | otherwise = findTimeStamp (timestamp + jumpBy) jumpBy buses

main = do
    (_, schedule) <- parseInput
    let buses = parseSchedule schedule
    print $ findTimeStamp 0 1 buses
