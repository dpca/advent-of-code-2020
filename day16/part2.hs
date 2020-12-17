{-# LANGUAGE TupleSections #-}

import Text.Parsec
import Text.Parsec.String
import Data.Either
import qualified Data.Map as M

type RuleName = String
type Rule = (RuleName, (Int, Int), (Int, Int))
type Ticket = [Int]
type Position = Int

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

findInvalid :: [Rule] -> Ticket -> Ticket
findInvalid rules = foldl (\acc x -> if isValid x then acc else acc ++ [x]) []
    where isValid x = any (ruleMatches x) rules

isValid :: [Rule] -> Ticket -> Bool
isValid rules ticket = null (findInvalid rules ticket)

narrowRules :: M.Map Position [Rule] -> Ticket -> M.Map Position [Rule]
narrowRules m ticket = foldl innerFold m $ zip [1..] ticket
    where innerFold acc (pos, x) = M.insert pos (findValidRules (acc M.! pos) x) acc
          findValidRules rules x = filter (ruleMatches x) rules

validRules :: [Rule] -> [Ticket] -> M.Map Position [RuleName]
validRules rules tickets = M.map (map (\(name, _, _) -> name)) finalRules
    where finalRules = foldl narrowRules initialRules tickets
          initialRules = M.fromList (map (, rules) [1..length (head tickets)])

processRuleMap :: M.Map Position [RuleName] -> M.Map RuleName Position
processRuleMap init = go init M.empty
    where go m acc
             | M.null m = acc
             | otherwise = go newMap (M.union acc knownRules)
           where
               (knownPositions, unknownPositions) = M.partition (\rules -> length rules == 1) m
               knownRules = M.fromList $ map (\(pos, rules) -> (head rules, pos)) $ M.toList knownPositions
               newMap = M.map (filter (\rule -> not $ M.member rule knownRules)) unknownPositions

findAnswer :: [Rule] -> Ticket -> [Ticket] -> Int
findAnswer rules myTicket nearbyTickets =
    let validTickets = filter (isValid rules) nearbyTickets
        ruleLocations = processRuleMap $ validRules rules validTickets
        departures = [
                         "departure location",
                         "departure station",
                         "departure platform",
                         "departure track",
                         "departure date",
                         "departure time"
                     ]
        departurePositions = map (ruleLocations M.!) departures
        myDepartures = map (\loc -> myTicket !! (loc - 1)) departurePositions
     in product myDepartures

main = do
    input <- readFile "input.txt"
    let (rules, myTicket, nearbyTickets) = fromRight ([], [], []) $ parse inputParser "input" input
    print $ findAnswer rules myTicket nearbyTickets
