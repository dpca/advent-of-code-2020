import Text.Parsec
import Text.Parsec.String

type RowCode = String
type ColCode = String
type SeatCode = Int

seatParser :: Parser (RowCode, ColCode)
seatParser = do
    row  <- count 7 $ oneOf "FB"
    col <- count 3 $ oneOf "LR"
    newline
    return (row, col)

parseSeats :: String -> [(RowCode, ColCode)]
parseSeats input = case parse (many seatParser) "" input of
                     Left _ -> []
                     Right x -> x

findBinary :: Int -> Char -> String -> Int
findBinary len match code = x
    where (x, _) = foldl processCode (0, len) code
          processCode (acc, here) code = if code == match
                                            then (acc + 2 ^ here, here - 1)
                                            else (acc, here - 1)

findSeat :: (RowCode, ColCode) -> SeatCode
findSeat (rowCode, colCode) = row * 8 + col
    where row = findBinary 6 'B' rowCode
          col = findBinary 2 'R' colCode

main = do
    input <- readFile "input.txt"
    let seatCodes = parseSeats input
    let rows = map findSeat seatCodes
    print $ maximum rows
