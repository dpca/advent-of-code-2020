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

lineParser :: Parser (Int, Int, Char, String)
lineParser = do
    lower <- read <$> many1 digit
    void $ char '-'
    upper <- read <$> many1 digit
    whitespace
    match <- anyChar
    void $ char ':'
    whitespace
    txt <- many1 anyChar
    return (lower, upper, match, txt)

isValid :: Either a (Int, Int, Char, String) -> Bool
isValid x = case x of
              Left _ -> False
              Right (lower, upper, match, txt)
                | count < lower -> False
                | count > upper -> False
                | otherwise -> True
                where count = length $ filter (== match) txt
