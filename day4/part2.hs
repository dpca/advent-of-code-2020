import Text.Read
import Text.Parsec
import Text.Parsec.String
import Control.Monad
import qualified Data.Map as M
import Data.Either

required :: [String]
required = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

validByr :: String -> Bool
validByr byr = case readMaybe byr :: Maybe Int of
                 Nothing -> False
                 Just x -> 1920 <= x && x <= 2002

validIyr :: String -> Bool
validIyr iyr = case readMaybe iyr :: Maybe Int of
                 Nothing -> False
                 Just x -> 2010 <= x && x <= 2020

validEyr :: String -> Bool
validEyr eyr = case readMaybe eyr :: Maybe Int of
                 Nothing -> False
                 Just x -> 2020 <= x && x <= 2030

hgtParser :: Parser (Int, String)
hgtParser = do
    val <- many1 digit
    unit <- string "cm" <|> string "in"
    return (read val, unit)

validHgt :: String -> Bool
validHgt hgt = case runParser hgtParser () "height" hgt of
                 Left _-> False
                 Right (val, unit) ->
                     if unit == "cm" then 150 <= val && val <= 193
                                     else 59 <= val && val <= 76

hclParser :: Parser ()
hclParser = do
    char '#'
    count 6 $ digit <|> oneOf ['a'..'f']
    return ()

validHcl :: String -> Bool
validHcl hcl = case runParser hclParser () "hair color" hcl of
                 Left _ -> False
                 Right _ -> True

validEcl :: String -> Bool
validEcl ecl = ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validPid :: String -> Bool
validPid pid = case runParser (count 9 digit) () "pid" pid of
                 Left _ -> False
                 Right _ -> length pid == 9

keyValParser :: Parser (String, String)
keyValParser = do
    key <- many1 $ alphaNum <|> char '#'
    char ':'
    val <- many1 $ alphaNum <|> char '#'
    oneOf " \n\t"
    return (key, val)

passportParser = do
    keyVals <- many keyValParser
    void $ char '\n'
    return $ M.fromList keyVals

isValidEntry :: (String, String) -> Bool
isValidEntry (key, val) =
    case key of
      "byr" -> validByr val
      "iyr" -> validIyr val
      "eyr" -> validEyr val
      "hgt" -> validHgt val
      "hcl" -> validHcl val
      "ecl" -> validEcl val
      "pid" -> validPid val
      "cid" -> True

type Passport = M.Map String String

validPassports passports = filter isValid parsedPassports
    where parsedPassports = fromRight [] passports
          isValid passport = allPresent passport && allValid passport
          allPresent passport = all (`M.member` passport) required
          allValid passport = all isValidEntry $ M.toList passport

main = do
    contents <- readFile "input.txt"
    let passports = runParser (many passportParser) () "input.txt" contents
    print $ length (validPassports passports)
