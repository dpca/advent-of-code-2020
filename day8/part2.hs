import Text.Parsec
import Text.Parsec.String
import Data.Either
import Data.List
import Data.Maybe
import Data.Set as Set

data Instruction = Nop Int | Acc Int | Jmp Int
    deriving (Show)

lineParser :: Parser Instruction
lineParser = do
    instruction <- many1 alphaNum
    space
    sign <- char '+' <|> char '-'
    num <- many1 digit
    newline
    let val = if sign == '+' then read num else -1 * read num
    return $ case instruction of
               "nop" -> Nop val
               "acc" -> Acc val
               "jmp" -> Jmp val

type Accumulator = Int
type Location = Int

programOk :: [Instruction] -> Accumulator -> Location -> Set Int -> Maybe Int
programOk lines acc loc seen
  | loc == length lines - 1 = Just acc
  | loc `member` seen = Nothing
  | otherwise = case lines !! loc of
                  Nop _ -> programOk lines acc (loc + 1) $ Set.insert loc seen
                  Acc x -> programOk lines (acc + x) (loc + 1) $ Set.insert loc seen
                  Jmp x -> programOk lines acc (loc + x) $ Set.insert loc seen

callProgram :: [Instruction] -> Maybe Int
callProgram lines = programOk lines 0 0 Set.empty

runWithSwap :: [Instruction] -> Int -> Instruction -> Maybe Int
runWithSwap lines idx swappedInstruction =
    let (before, _ : after) = Data.List.splitAt idx lines
     in callProgram (before ++ [swappedInstruction] ++ after)

fixProgram :: [Instruction] -> Int
fixProgram lines =
    let tryAltProgram (_,   Acc _) = Nothing
        tryAltProgram (idx, Nop x) = runWithSwap lines idx (Jmp x)
        tryAltProgram (idx, Jmp x) = runWithSwap lines idx (Nop x)
        fixedProgram = zipWith (curry tryAltProgram) [0..] lines
     in fromJust . fromJust $ find isJust fixedProgram

main = do
    input <- readFile "input.txt"
    let lines = fromRight [] $ parse (many lineParser) "input" input
    print $ fixProgram lines
