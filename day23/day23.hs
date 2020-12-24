import Data.Char
import Data.Foldable
import qualified Data.IntMap as M

createCircle :: [Int] -> M.IntMap Int
createCircle = go M.empty
    where go acc [x] = M.insert x 4 acc
          go acc (x:y:xs) = go (M.insert x y acc) (y:xs)

findDestinationCup :: Int -> [Int] -> Int -> Int
findDestinationCup current pickedUp maxNum = go (current - 1) pickedUp maxNum
    where go 0 _ maxNum = go maxNum pickedUp maxNum
          go n pickedUp maxNum = case find (== n) pickedUp of
                                   Just _  -> go (n - 1) pickedUp maxNum
                                   Nothing -> n

runGame :: Int -> Int -> M.IntMap Int -> Int -> M.IntMap Int
runGame 0 _ cups _ = cups
runGame n current cups maxNum =
    let c1 = cups M.! current
        c2 = cups M.! c1
        c3 = cups M.! c2
        c4 = cups M.! c3
        destination = findDestinationCup current [c1, c2, c3] maxNum
        postDestination = cups M.! destination
        newCups = M.fromList [(current, c4), (destination, c1), (c3, postDestination)] `M.union` cups
     in runGame (n - 1) c4 newCups maxNum

cupsFrom1 :: M.IntMap Int -> [Int]
cupsFrom1 cups = reverse $ go cups [] (cups M.! 1)
    where go _ acc 1 = acc
          go cups acc num = go cups (num:acc) (cups M.! num)

main = do
    let input = map digitToInt "467528193"
    let start = head input
    let max1 = maximum input
    let cups = createCircle input
    let part1 = runGame 100 start cups max1
    print $ "Part 1: " ++ concatMap show (cupsFrom1 part1)

    let max2 = 1000000
    let moreCups = createCircle $ input ++ [10..max2]
    let part2 = runGame 10000000 start moreCups max2
    let part2_1 = part2 M.! 1
    let part2_2 = part2 M.! part2_1
    print $ "Part 2: " ++ show (part2_1 * part2_2)
