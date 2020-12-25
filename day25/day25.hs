import qualified Data.IntMap as M

transform :: Int -> M.IntMap Int -> Int -> (Int, M.IntMap Int)
transform iter memo _ | iter == 0 = (1, memo)
transform iter memo subject =
    let lookup = M.lookup iter memo
        (prevVal, newMemo) = transform (iter - 1) memo subject
        newVal = (prevVal * subject) `rem` 20201227
     in case lookup of
          Just x  -> (x, memo)
          Nothing -> (newVal, M.insert iter newVal newMemo)

findLoopSize :: Int -> Int -> Int
findLoopSize subject pubkey = go M.empty 1
    where go memo n =
            let (val, newMemo) = transform n memo subject
             in if val == pubkey then n else go newMemo (n + 1)

main = do
    let cardPubKey = 8458505
    let doorPubKey = 16050997
    let cardLoopSize = findLoopSize 7 cardPubKey
    let (encryptionKey, _) = transform cardLoopSize M.empty doorPubKey
    print $ "Part 1: " ++ show encryptionKey
