#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
import Control.Parallel.Strategies
import qualified Data.Map as M

type JustMonkey = String
type Val = Int
data Operation = Mul | Add | Sub | Div deriving (Show)
type MonkeyVal = M.Map JustMonkey Val
data Action = Value Val JustMonkey | Action JustMonkey Operation JustMonkey JustMonkey deriving (Show)

main :: IO ()
main = interact $ show . step M.empty . parMap rpar parsein . lines

step :: MonkeyVal -> [Action] -> Int
step mv [] =case M.lookup "root" mv of
        Just a -> a
        Nothing -> error "Didn't find what you were looking for."
step mv ((Value v m):rest) =
    let mv' = M.insert m v mv
    in step mv' rest
step mv (hd@(Action m1 op m2 m3):rest) =
    case (M.lookup m2 mv, M.lookup m3 mv) of
        (Just a, Just b) -> step mv ((Value (compute op a b) m1):rest)
        otherwise -> step mv (rest ++ [hd]) -- Move to back

compute :: Operation -> Int -> Int -> Int
compute Mul a b = a * b
compute Add a b = a + b
compute Sub a b = a - b
compute Div a b = a `div` b

parsein :: String -> Action
parsein input = case parse (try parseValue <|> try parseAction) "parsein" input of
    Left err -> error $ show err
    Right a -> a

parseValue :: Parsec String () Action
parseValue = do --czpt: 5
    monkey <- many1 lower
    ______ <- string ": "
    val    <- read <$> many1 digit
    return $ Value val monkey

parseAction :: Parsec String () Action
parseAction = do -- sdmf: gbvc * gvmh
    monkey <- many1 lower
    ______ <- string ": "
    id1    <- many1 lower
    op     <- parseOperation
    id2    <- many1 lower
    return $ Action monkey op id1 id2

parseOperation :: Parsec String () Operation
parseOperation = try parseMul <|> try parseDiv <|> try parseAdd <|> try parseSub

parseMul = string " * " >> return Mul
parseDiv = string " / " >> return Div
parseAdd = string " + " >> return Add
parseSub = string " - " >> return Sub
