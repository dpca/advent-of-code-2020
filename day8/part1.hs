import Text.Parsec
import Text.Parsec.String
import Data.Either
import Data.Set as Set

data Instruction = Nop | Acc Int | Jmp Int
    deriving (Show)

lineParser :: Parser Instruction
lineParser = do
    instruction <- many1 letter
    space
    sign <- char '+' <|> char '-'
    num <- many1 digit
    newline
    let val = if sign == '+' then read num else -1 * read num
    return $ case instruction of
               "nop" -> Nop
               "acc" -> Acc val
               "jmp" -> Jmp val

type Accumulator = Int
type Location = Int

executeProgram :: [Instruction] -> Accumulator -> Location -> Set Int -> Int
executeProgram lines acc loc seen =
    if loc `member` seen
       then acc
       else case lines !! loc of
              Nop -> executeProgram lines acc (loc + 1) $ insert loc seen
              Acc x -> executeProgram lines (acc + x) (loc + 1) $ insert loc seen
              Jmp x -> executeProgram lines acc (loc + x) $ insert loc seen

callProgram :: [Instruction] -> Int
callProgram lines = executeProgram lines 0 0 Set.empty

main = do
    input <- readFile "input.txt"
    let lines = fromRight [] $ parse (many lineParser) "input" input
    print $ callProgram lines
