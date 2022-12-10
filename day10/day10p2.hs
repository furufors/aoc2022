#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
import Data.List.Split
data Instruction = Add Int | Noop deriving (Show)

main :: IO ()
main = interact $ intercalate "\n" . toScreen . execute [] . map parse . lines

toScreen :: [Int] -> [String]
toScreen is = chunksOf 40 . map visible . init $ (0,1):zip is [1..]

visible (p,i) = if p - (i `mod` 40) `elem` [-1,0,1] then '#' else '.'

numbers :: [Int] -> [Int]
numbers is = [i * is !! (i-2) | i <- [20,60,100,140,180,220]]

execute :: [Int] -> [Instruction] -> [Int]
execute [] (Noop:is) = execute [1] is
execute [] ((Add x):is) = execute [1, 1+x] is
execute as [] = as
execute as (Noop:is)    = execute (as ++ [last as]) is
execute as ((Add x):is) = execute (as ++ [last as, last as + x]) is

parse :: String -> Instruction
parse "noop" = Noop
parse x = Add . read . tail $ dropWhile (/=' ') x