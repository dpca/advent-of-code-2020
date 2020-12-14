import Text.Parsec
import Text.Parsec.String
import Data.Either
import qualified Data.Map as M

type Mask = String
type MemAddress = Int
type MemValue = Int
type Instruction = (MemAddress, MemValue)
type Bits = [Int]

maskParser :: Parser String
maskParser = do
    string "mask = "
    mask <- many1 (digit <|> char 'X')
    newline
    return mask

memParser :: Parser (Int, Int)
memParser = do
    string "mem["
    address <- many1 digit
    string "] = "
    value <- many1 digit
    newline
    return (read address, read value)

inputGroups :: Parser (Mask, [Instruction])
inputGroups = do
    mask <- maskParser
    mems <- manyTill memParser (try (lookAhead (string "mask" <|> (eof >> return []))))
    return (mask, mems)

toBits :: Int -> Int -> Bits
toBits 0    _ = []
toBits size 0 = [0 | _ <- [1..size]]
toBits size x =
    let bitPos = size - 1
        bitVal = 2^bitPos
        divNum = x `div` bitVal
     in if divNum == 0
           then 0 : toBits bitPos x
           else 1 : toBits bitPos (x - divNum*bitVal)

unBits :: Int -> Bits -> Int
unBits 0 _  = 0
unBits _ [] = 0
unBits size (x:xs) =
    let bitPos = size - 1
        bitVal = 2^bitPos
        num    = x * bitVal
     in num + unBits bitPos xs

applyMask :: Mask -> [MemAddress] -> [[MemAddress]]
applyMask = go [[]]
    where go acc [] [] = acc
          go acc (m:ms) (b:bs)
            | m == '0' = go (map (\x -> x ++ [b]) acc) ms bs
            | m == '1' = go (map (\x -> x ++ [1]) acc) ms bs
            | m == 'X' = go (concatMap (\x -> [x ++ [0], x ++ [1]]) acc) ms bs

runGroup :: M.Map MemAddress MemValue -> (Mask, [Instruction]) -> M.Map MemAddress MemValue
runGroup m (mask, mems) = foldl apply m mems
    where apply acc (addr, val) = foldl (\m newAddr -> M.insert newAddr val m) acc $ alteredMasks addr
          alteredMasks addr = map (unBits 36) $ applyMask mask (toBits 36 addr)

getAnswer :: [(Mask, [Instruction])] -> Int
getAnswer groups = M.foldl (+) 0 answer
    where answer = foldl runGroup M.empty groups

main = do
    input <- readFile "input.txt"
    let groups = fromRight [] $ parse (many1 inputGroups) "input" input
    print $ getAnswer groups
