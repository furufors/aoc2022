#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
type Rucksack = (String,String)

main :: IO ()
main = interact $ show . sum . map (scoreDuplicate . findDuplicate . toRucksack) . lines

toRucksack :: String -> Rucksack
toRucksack s = let len = length s `div` 2
               in (take len s, drop len s)

findDuplicate :: Rucksack -> Char
findDuplicate ((c:cs),s2) = if elem c s2 then c else findDuplicate (cs,s2)
findDuplicate ([],s2) = error "no duplicate found"

scoreDuplicate :: Char -> Int
scoreDuplicate c = score c scores
    where
        score c1 ((c2,i):ss) = if c1 == c2 then i else score c1 ss

scores :: [(Char,Int)]
scores = zip ['a'..'z'] [1..26] ++ zip ['A'..'Z'] [27..52]