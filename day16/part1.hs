import Text.Parsec
import Text.Parsec.String
import Data.Either

type Rule = (String, (Int, Int), (Int, Int))
type Ticket = [Int]

ruleParser :: Parser Rule
ruleParser = do
    name <- manyTill (alphaNum <|> space) (try (string ": "))
    low1 <- many1 digit
    char '-'
    high1 <- many1 digit
    string " or "
    low2 <- many1 digit
    char '-'
    high2 <- many1 digit
    newline
    return (name, (read low1, read high1), (read low2, read high2))

ticketParser :: Parser Ticket
ticketParser = do
    digits <- sepBy (many1 digit) (char ',')
    newline
    return $ map read digits

inputParser :: Parser ([Rule], Ticket, [Ticket])
inputParser = do
    rules <- manyTill ruleParser newline
    string "your ticket:"
    newline
    yourTicket <- ticketParser
    newline
    string "nearby tickets:"
    newline
    nearbyTickets <- many1 ticketParser
    return (rules, yourTicket, nearbyTickets)

ruleMatches :: Int -> Rule -> Bool
ruleMatches x (_, (l1, h1), (l2, h2)) = ((l1 <= x) && (x <= h1)) || ((l2 <= x) && (x <= h2))

findInvalid :: [Rule] -> Ticket -> [Int]
findInvalid rules = foldl (\acc x -> if isValid x then acc else acc ++ [x]) []
    where isValid x = any (ruleMatches x) rules

main = do
    input <- readFile "input.txt"
    let (rules, _, nearbyTickets) = fromRight ([], [], []) $ parse inputParser "input" input
    print $ sum $ findInvalid rules (concat nearbyTickets)
