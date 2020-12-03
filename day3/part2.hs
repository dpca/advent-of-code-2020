type TobogganMap = [String]
type Slope = (Int, Int) -- right, down

countTrees :: TobogganMap -> Slope -> Int
countTrees tobogganMap (right, down) = finalTrees
    where
        (finalTrees, _, _) = foldl processLine (0, 0, 0) tobogganMap
        processLine (trees, x, y) line =
            if mod y down == 0
               then (trees + treeCheck line x, moveRight line x right, y + 1)
               else (trees, x, y + 1)
        treeCheck line x = if line !! x == '#' then 1 else 0
        moveRight line x right = mod (x + right) (length line)

main = do
    tobogganMap <- lines <$> readFile "input.txt"
    let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    let trees = product $ map (countTrees tobogganMap) slopes
    print trees

