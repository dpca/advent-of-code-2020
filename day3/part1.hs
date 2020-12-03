main = do
    contents <- lines <$> readFile "input.txt"
    let len = length $ head contents
    let (result, _) = foldl (\(acc, here) line -> (acc + (if line !! here == '#' then 1 else 0), mod (here + 3) len)) (0, 0) contents
    print result
