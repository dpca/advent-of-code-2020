import Text.Parsec
import Text.Parsec.String
import Data.Either
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

type BagName = String
type BagMap = M.Map BagName (S.Set BagName)

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

invertBags :: [(BagName, [(Int, BagName)])] -> BagMap
invertBags = foldl (\acc (outerName, innerBags) -> collectBags outerName acc innerBags) M.empty
    where collectBags outerName = foldl (\m (_, innerName) -> insertBag m outerName innerName)
          insertBag m outerName innerName = M.insert innerName (mergeBags m outerName innerName) m
          mergeBags m outerName innerName = S.insert outerName (lookup m innerName)
          lookup m innerName = fromMaybe S.empty $ M.lookup innerName m

findAllPossibilities :: BagMap -> BagName -> S.Set BagName
findAllPossibilities bagMap name =
    let mergeInnerBags set innerBag = S.union set $ findAllPossibilities bagMap innerBag
    in  case M.lookup name bagMap of
          Just x  -> S.foldl mergeInnerBags (S.fromList [name]) x
          Nothing -> S.fromList [name]

main = do
    input <- readFile "input.txt"
    let bags = fromRight [] $ parse (many ruleParser) "input" input
    let bagMap = invertBags bags
    print $ length (findAllPossibilities bagMap "shiny gold") - 1
