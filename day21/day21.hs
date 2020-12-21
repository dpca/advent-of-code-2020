import Text.Parsec
import Text.Parsec.String
import Data.List
import Data.Ord
import qualified Data.Map as M

parseIt :: Parser a -> String -> a
parseIt p x = case parse p "" x of
                Left  err -> error (show err)
                Right res -> res

type Ingredient = String
type Allergen = String
type FoodIdx = Int
data Food = Food { idx :: FoodIdx, ingredients :: [Ingredient], allergens :: [Allergen] } deriving (Show)

inputParser :: FoodIdx -> Parser Food
inputParser idx = do
    ingredients <- many1 letter `endBy` space
    allergens <- between (char '(') (char ')') (string "contains " *> many1 letter `sepBy` string ", ")
    return Food { idx = idx, ingredients = ingredients, allergens = allergens }

foodsByIdx :: [Food] -> M.Map FoodIdx Food
foodsByIdx = foldl (\acc food@Food { idx = idx } -> M.insert idx food acc) M.empty

foodsByAllergen :: [Food] -> M.Map Allergen [FoodIdx]
foodsByAllergen = go M.empty
    where go = foldl (\acc Food { idx = idx, allergens = allergens } -> innerFold idx acc allergens)
          innerFold idx acc allergens = foldl (\m a -> M.insertWith (++) a [idx] m) acc allergens

findAllergens :: M.Map FoodIdx Food -> [FoodIdx] -> [Ingredient]
findAllergens = go []
    where go acc _ [] = acc
          go []  foods (idx:xs) = go (ingredients (foods M.! idx)) foods xs
          go acc foods (idx:xs) = go (acc `intersect` ingredients (foods M.! idx)) foods xs

narrowAllergens :: M.Map Allergen [Ingredient] -> [(Ingredient, Allergen)]
narrowAllergens allergens =
    let initialAllergens = M.toList allergens
        allergensWithout acc = map (\(n, lst) -> (n, lst \\ map fst acc)) initialAllergens
        go acc [] = acc
        go acc ((name, lst):_) | length lst == 1 = go newAcc (allergensWithout newAcc)
            where newAcc = (head lst, name) : acc
        go acc (_:xs) = go acc xs
     in go [] initialAllergens

findKnownAllergens :: [Food] -> [(Ingredient, Allergen)]
findKnownAllergens foods =
    let foodIdx = foodsByIdx foods
        foodAllergens = foodsByAllergen foods
        ingredientsByAllergen = M.map (findAllergens foodIdx) foodAllergens
     in narrowAllergens ingredientsByAllergen

main = do
    input <- readFile "input.txt"
    let foods = zipWith (parseIt . inputParser) [0..] (lines input)
    let knownAllergens = findKnownAllergens foods
    let okIngredients = concatMap (\food -> ingredients food \\ map fst knownAllergens) foods
    let dangerousList = map fst $ sortBy (comparing snd) knownAllergens
    print $ "Part 1: " ++ show (length okIngredients)
    print $ "Part 2: " ++ intercalate "," dangerousList
