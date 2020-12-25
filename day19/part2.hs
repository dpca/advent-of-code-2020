import Text.Parsec
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe

data Rule = Leaf Char | Pointers [[Int]] deriving (Show)

parseLine :: String -> (Int, Rule)
parseLine line = (read ruleNum, parseRules (tail str))
    where (ruleNum:str:_) = splitOn ":" line
          parsePointers rules = map (map read . splitOn " ") $ splitOn " | " rules
          parseRules rules =
              case parse (between (char '"') (char '"') letter) "" rules of
                Left _  -> Pointers $ parsePointers rules
                Right l -> Leaf l

checkRule :: Int -> M.Map Int Rule -> [String] -> Maybe [String]
checkRule _ _ [] = Nothing
checkRule num rules remainders =
    let rule = rules M.! num
        checkGroup rem [] = Just rem
        checkGroup rem (n:lst) = case checkRule n rules rem of
                                   Just x  -> checkGroup x lst
                                   Nothing -> Nothing
        checkPtrs groups =
            foldr (\group acc -> case checkGroup remainders group of
                                   Just x -> acc ++ x
                                   Nothing -> acc) [] groups
        checkLeaf _ [] a = a
        checkLeaf char (x:xs) a = if x == char then a ++ [xs] else a
     in case rule of
          Pointers ptrs -> Just $ checkPtrs ptrs
          Leaf char -> case foldr (checkLeaf char) [] remainders of
                         [] -> Nothing
                         x  -> Just x

modifyRules :: M.Map Int Rule -> M.Map Int Rule
modifyRules rules =
    M.insert 11 (Pointers [[42, 31], [42, 11, 31]]) $
        M.insert 8 (Pointers [[42], [42, 8]]) rules

getMatches :: String -> [[String]]
getMatches input =
    let (instructions:messages:_) = splitOn "\n\n" input
        rules =  M.fromList $ map parseLine (lines instructions)
        modifiedRules = modifyRules rules
        matches = mapMaybe (\line -> checkRule 0 modifiedRules [line]) $ lines messages
        filterMatches matches = filter (elem "") matches
     in filterMatches matches

main = do
    input <- readFile "input.txt"
    print $ length (getMatches input)
