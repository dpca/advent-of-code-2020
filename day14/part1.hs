import Text.Parsec
import Text.Parsec.String
import Data.Either
import qualified Data.Map as M

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

inputGroups :: Parser (String, [(Int, Int)])
inputGroups = do
    mask <- maskParser
    mems <- manyTill memParser (try (lookAhead (string "mask" <|> (eof >> return []))))
    return (mask, mems)

toBits :: Int -> Int -> [Int]
toBits 0    _ = []
toBits size 0 = [0 | _ <- [1..size]]
toBits size x =
    let bitPos = size - 1
        bitVal = 2^bitPos
        divNum = x `div` bitVal
     in if divNum == 0
           then 0 : toBits bitPos x
           else 1 : toBits bitPos (x - divNum*bitVal)

unBits :: Int -> [Int] -> Int
unBits 0 _  = 0
unBits _ [] = 0
unBits size (x:xs) =
    let bitPos = size - 1
        bitVal = 2^bitPos
        num    = x * bitVal
     in num + unBits bitPos xs

applyMask :: String -> [Int] -> [Int]
applyMask = zipWith apply
    where apply 'X' b = b
          apply '0' _ = 0
          apply '1' _ = 1

runGroup :: M.Map Int Int -> (String, [(Int, Int)]) -> M.Map Int Int
runGroup map (mask, mems) = foldl (\m (addr, val) -> M.insert addr (apply val) m) map mems
    where apply val = unBits 36 $ applyMask mask $ toBits 36 val

main = do
    input <- readFile "input.txt"
    let groups = fromRight [] $ parse (many1 inputGroups) "input" input
    let answer = foldl runGroup M.empty groups
    print $ M.foldl (+) 0 answer
