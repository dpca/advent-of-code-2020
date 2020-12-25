import Data.Maybe
import qualified Data.IntMap as M

transform :: Int -> M.IntMap Int -> Int -> Int
transform iter _ _ | iter == 0 = 1
transform iter memo subject =
    let lookup = M.lookup iter memo
        prevVal = transform (iter - 1) memo subject
        newVal = (prevVal * subject) `rem` 20201227
     in fromMaybe newVal lookup

findLoopSize :: Int -> Int -> Int
findLoopSize subject pubkey = go M.empty 1
    where go memo n =
            let val = transform n memo subject
                newMemo = M.insert n val memo
             in if val == pubkey then n else go newMemo (n + 1)

main = do
    let cardPubKey = 8458505
    let doorPubKey = 16050997
    let cardLoopSize = findLoopSize 7 cardPubKey
    let encryptionKey = transform cardLoopSize M.empty doorPubKey
    print $ "Part 1: " ++ show encryptionKey
