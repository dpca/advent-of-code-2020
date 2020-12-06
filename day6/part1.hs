import Text.Parsec
import Text.Parsec.String
import Data.List
import Data.Either

answerParser :: Parser String
answerParser = do
    words <- many1 alphaNum
    newline
    return words

groupParser :: Parser [String]
groupParser = do
    group <- many answerParser
    newline
    return group

main = do
    input <- readFile "input.txt"
    let groups = fromRight [] $ parse (many groupParser) "input" input
    print $ sum $ map (length . nub . concat) groups
