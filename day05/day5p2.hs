#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Char
type Stacks = [String]
type Instruction = (Int,Int,Int)
type Instructions = [Instruction]

main :: IO ()
main = interact $ show . topLayer . fst . applyInstructions . parse . lines

game = ["VQWMBNZC","BCWRZH","JRQF","TMNFHWSZ","PQNLWFG","WPL","JQCGRDBV","WBNQZ","JTGCFLH"]
testcase = ["NZ","DCM","P"]

parse :: [String] -> (Stacks,Instructions)
parse ss = let first = takeWhile (/="") ss
               second = drop 10 ss
               count = maximum . map (read :: String -> Int) . words . last $ first
           in (game, map parseInstructions second)

parseInstructions :: String -> Instruction
parseInstructions s = let a = takeWhile isDigit $ drop 5 s
                          b = takeWhile isDigit $ drop 6 $ dropWhile isDigit $ drop 5 s
                          c = takeWhile isDigit $ drop 4 $ dropWhile isDigit $ drop 6 $ dropWhile isDigit $ drop 5 s
                      in (read a, read b, read c)

applyInstructions :: (Stacks,Instructions) -> (Stacks,Instructions)
applyInstructions (stacks,[]) = (stacks,[])
applyInstructions (stacks,((m,f,t):rest)) =
    let (letters, s1) = takeFrom m f stacks
        new = add letters t s1
    in applyInstructions (new,rest)

takeFrom :: Int -> Int -> Stacks -> (String,[String])
takeFrom m f ss = (take m (ss !! (f-1)), take (f-1) ss ++ [drop m (ss !! (f-1))] ++ drop f ss)

add :: String -> Int -> Stacks -> Stacks
add l f ss = take (f-1) ss ++ [l ++ (ss !! (f-1))] ++ drop f ss

topLayer :: Stacks -> String
topLayer = map head