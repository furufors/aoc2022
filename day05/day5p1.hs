#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Char
type Stacks = [String]
type Instruction = (Int,Int,Int)
type Instructions = [Instruction]

main :: IO ()
main = interact $ show . map head . fst . applyInstructions . parse . lines

parse :: [String] -> (Stacks,Instructions)
parse ss = let first = init $ takeWhile (/="") ss
               second = tail $ dropWhile (/="") ss
               count = maximum . map (read :: String -> Int) . words . last . takeWhile (/="") $ ss
           in (startingStack count first, map parseInstructions second)

startingStack :: Int -> [String] -> Stacks
startingStack bins ss =
    let height = length ss
        out = [ [char | y <- [0..(height-1)], x < length (ss!!y) , let char = ss!!y!!x, char /= '.']
                | i <- [1..bins], let x = -3 + 4 * i]
    in map (dropWhile isSpace) out

parseInstructions :: String -> Instruction
parseInstructions s = let a = takeWhile isDigit $ drop 5 s
                          b = takeWhile isDigit $ drop 6 $ dropWhile isDigit $ drop 5 s
                          c = takeWhile isDigit $ drop 4 $ dropWhile isDigit $ drop 6 $ dropWhile isDigit $ drop 5 s
                      in (read a, read b, read c)

applyInstructions :: (Stacks,Instructions) -> (Stacks,Instructions)
applyInstructions (stacks,[]) = (stacks,[])
applyInstructions (stacks,((m,f,t):rest)) =
    let (letters, s1) = takeFrom m f stacks
    in applyInstructions (add letters t s1,rest)

takeFrom :: Int -> Int -> Stacks -> (String,[String])
takeFrom m f ss = (take m (ss !! (f-1)), take (f-1) ss ++ [drop m (ss !! (f-1))] ++ drop f ss)

add :: String -> Int -> Stacks -> Stacks
add l f ss = take (f-1) ss ++ [reverse l ++ (ss !! (f-1))] ++ drop f ss