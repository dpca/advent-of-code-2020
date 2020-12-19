import Text.Parsec
import Text.Parsec.String
import Data.Either

data AST = Val Int
         | Plus
         | Times
         | Paren [AST]
         deriving (Show)

parseInput :: Parser AST
parseInput = Paren <$> many1 parseTree
    where parseTree = leaf <|> try add <|> try mult <|> node
          leaf = Val . read <$> many1 digit
          add  = Plus  <$  string " + "
          mult = Times <$  string " * "
          node = Paren <$> between (char '(') (char ')') (many1 parseTree)

data Tree = Leaf Int
          | Add  Tree Tree
          | Mult Tree Tree
          deriving (Show)

makeTree :: AST -> Tree
makeTree (Val x) = Leaf x
makeTree (Paren lst) = mult [] lst
    where mult prev (Times:xs) = Mult (mult prev []) (mult [] xs)
          mult prev (x:xs)     = mult (prev ++ [x]) xs
          mult prev []         = add [] prev
          add prev (Plus:xs)   = Add (add prev []) (add [] xs)
          add prev (x:xs)      = add (prev ++ [x]) xs
          add [Val x] []       = Leaf x
          add [Paren l] []     = makeTree $ Paren l

execute :: Tree -> Int
execute (Leaf x) = x
execute (Add x y) = execute x + execute y
execute (Mult x y) = execute x * execute y

executeLine :: String -> Int
executeLine line = execute tree
    where tree = makeTree ast
          ast  = fromRight (Paren []) $ parse parseInput "input" line

main = do
    input <- readFile "input.txt"
    print $ sum $ map executeLine (lines input)
