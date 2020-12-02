import Text.Parsec
import Text.Parsec.String
import Control.Monad

main = do
    contents <- readFile "input.txt"
    let parsedLines = map (runParser lineParser () "input.txt") $ lines contents
    let validLines = filter isValid parsedLines
    print $ length validLines

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lineParser :: Parser (Char, Char, Char)
lineParser = do
    lower <- read <$> many1 digit
    void $ char '-'
    upper <- read <$> many1 digit
    whitespace
    match <- anyChar
    void $ char ':'
    whitespace
    void $ count (lower - 1) anyChar
    firstChar <- anyChar
    void $ count (upper - lower - 1) anyChar
    secondChar <- anyChar
    return (match, firstChar, secondChar)

isValid :: Either a (Char, Char, Char) -> Bool
isValid x = case x of
              Left _ -> False
              Right (match, firstChar, secondChar)
                | match == firstChar && match == secondChar -> False
                | match /= firstChar && match /= secondChar -> False
                | otherwise -> True
