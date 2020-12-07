import Text.Parsec
import Text.Parsec.String
import Data.Either
import Data.Maybe
import qualified Data.Map as M

type BagName = String
type BagMap = M.Map BagName [(Int, BagName)]

bagName :: Parser String
bagName = many1 $ oneOf ['a'..'z'] <|> space

innerBag :: Parser (Int, BagName)
innerBag = do
    numBags <- many1 digit
    space
    name <- manyTill anyChar (try (string " bag"))
    many $ char 's'
    string ", " <|> string "."
    return (read numBags, name)

noBag :: Parser [(Int, BagName)]
noBag = do
    string "no other bags."
    return []

ruleParser :: Parser (BagName, [(Int, BagName)])
ruleParser = do
    name <- manyTill anyChar (try (string " bags contain "))
    innerBags <- many1 innerBag <|> noBag
    newline
    return (name, innerBags)

findAllBags :: String -> BagMap -> Int
findAllBags outerName bagMap = sum bagsWithin
    where bagsWithin = map (\(num, name) -> num + num * findAllBags name bagMap) $ bagLookup outerName
          bagLookup name = fromMaybe [] $ M.lookup name bagMap

main = do
    input <- readFile "input.txt"
    let bags = fromRight [] $ parse (many ruleParser) "input" input
    let bagMap = M.fromList bags
    print $ findAllBags "shiny gold" bagMap
