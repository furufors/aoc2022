#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
type Assignment = ([Int], [Int])

main :: IO ()
main = interact $ show . sum . map (fullOverlap . parse) . lines

parse :: String -> Assignment
parse s = let ( a,  (_:b)) = span (/=',') s
              (a1, (_:a2)) = span (/='-') a
              (b1, (_:b2)) = span (/='-') b
          in ([(read a1)..(read a2)],[(read b1)..(read b2)])

fullOverlap :: Assignment -> Int
fullOverlap (as, bs) = if overLap as bs then 1 else 0

overLap :: [Int] -> [Int] -> Bool
overLap as bs = all (\a -> a `elem` bs) as || all (\b -> b `elem` as) bs