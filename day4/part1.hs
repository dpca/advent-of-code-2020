import Text.Parsec
import Text.Parsec.String
import Control.Monad
import qualified Data.Map as M
import Data.Either

required = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

keyValParser :: Parser (String, String)
keyValParser = do
    key <- many1 $ alphaNum <|> char '#'
    char ':'
    val <- many1 $ alphaNum <|> char '#'
    oneOf " \n\t"
    return (key, val)

passportParser = do
    keyVals <- many keyValParser
    char '\n'
    return $ M.fromList keyVals

contentParser = many passportParser

validPassports passports = length $ filter isValid (fromRight [] passports)
    where isValid passport = all (`M.member` passport) required

main = do
    contents <- readFile "input.txt"
    let passports = runParser contentParser () "input.txt" contents
    print $ validPassports passports
