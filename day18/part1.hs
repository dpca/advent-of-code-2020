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
makeTree (Paren lst) = goParen (reverse lst)
    where goParen (x:Plus:xs)  = Add (makeTree x) (goParen xs)
          goParen (x:Times:xs) = Mult (makeTree x) (goParen xs)
          goParen [x]          = makeTree x

execute :: Tree -> Int
execute (Leaf x)   = x
execute (Add x y)  = execute x + execute y
execute (Mult x y) = execute x * execute y

executeLine :: String -> Int
executeLine line =
    let ast  = fromRight (Paren []) $ parse parseInput "input" line
        tree = makeTree ast
     in execute tree

main = do
    input <- readFile "input.txt"
    print $ sum $ map executeLine (lines input)
