#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
import Control.Parallel.Strategies
import qualified Data.Map as M

type JustMonkey = String
data Operation = Mul | Add | Sub | Div | Eqs deriving (Show)
data Action = Value Int JustMonkey | Action JustMonkey Operation JustMonkey JustMonkey
data Resolver = Val Int | Res (Int -> Int)

main :: IO ()
main = interact $ show . runResolver . parMap rpar parsein . ("root: bjgs = tjtt":) . filter (/="root: bjgs + tjtt") . lines

runResolver :: [Action] -> Int
runResolver as = let (Val a) = toResolver as "root" in a

app :: (Int -> Int) -> Resolver -> Resolver
app g (Res f) = Res $ f . g

toResolver :: [Action] -> String -> Resolver
toResolver _ "humn" = Res id
toResolver ns n = case findAction ns n of
    Value i _ -> Val i
    Action _ op m1 m2 -> resolver op (toResolver ns m1) (toResolver ns m2)

findAction :: [Action] -> String -> Action
findAction [] _ = error "No findAction match."
findAction ((Value v an):as) tn = if an == tn then Value v an else findAction as tn
findAction ((Action an op m1 m2):as) tn = if an == tn then Action an op m1 m2 else findAction as tn

compute :: Operation -> Int -> Int -> Int
compute Mul a b = a * b
compute Add a b = a + b
compute Sub a b = a - b
compute Div a b = a `div` b
compute Eqs a b = error "= Should not appear in compute."

resolver :: Operation -> Resolver -> Resolver -> Resolver
resolver op  (Val a) (Val b) = Val $ compute op a b
resolver Eqs (Res f) (Val a) = Val $ f a
resolver Eqs (Val a) (Res f) = Val $ f a
resolver Mul (Val a)  r      = app (\x -> x `div` a) r
resolver Mul r       (Val a) = app (\x -> x `div` a) r
resolver Div (Val a) r       = app (\x -> a `div` x) r
resolver Div r       (Val a) = app (\x -> a * x) r
resolver Add (Val a) r       = app (\x -> x - a) r
resolver Add r       (Val a) = app (\x -> x - a) r
resolver Sub (Val a) r       = app (\x -> a - x) r
resolver Sub r       (Val a) = app (\x -> a + x) r

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
parseOperation = try parseMul <|> try parseDiv <|> try parseAdd <|> try parseSub <|> try parseEqs

parseMul = string " * " >> return Mul
parseDiv = string " / " >> return Div
parseAdd = string " + " >> return Add
parseSub = string " - " >> return Sub
parseEqs = string " = " >> return Eqs
