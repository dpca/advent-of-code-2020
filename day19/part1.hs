import Text.Parsec
import Text.Parsec.String
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Functor.Identity

parseIt :: Parser a -> String -> a
parseIt p x = case parse p "" x of
                Left  err -> error (show err)
                Right res -> res

parseRules :: Parser (Int, String)
parseRules = do
    ruleNum <- many1 digit
    string ": "
    rules <- many anyChar
    return (read ruleNum, rules)

subParser :: M.Map Int String -> String -> ParsecT String () Identity String
subParser rules subRule = foldl1 (*>) (parseIt parsers subRule)
    where parsers = (dig <|> str) `sepBy` space
          dig = parseRule rules . read <$> many1 digit
          str = string <$> between (char '"') (char '"') (many1 letter)

parseRule :: M.Map Int String -> Int -> Parser String
parseRule rules num =
    let thisRule = rules M.! num
        subRules = splitOn " | " thisRule
        subParsers = map (subParser rules) subRules
     in foldl1 (\l r -> try l <|> try r) subParsers

filterRights :: [Either ParseError x] -> [x]
filterRights [] = []
filterRights (Left _:xs) = filterRights xs
filterRights (Right x:xs) = x:filterRights xs

getMatches :: String -> [String]
getMatches input =
    let (instructions:messages:_) = splitOn "\n\n" input
        rules = M.fromList $ map (parseIt parseRules) (lines instructions)
        fullParser = parseRule rules 0 <* eof
     in filterRights $ map (parse fullParser "") (lines messages)

main = do
    input <- readFile "input.txt"
    print $ length (getMatches input)
