#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
type Rucksacks = (String,String,String)

main :: IO ()
main = interact $ show . sum . map (scoreDuplicate . findDuplicate) . toRucksack . lines

toRucksack :: [String] -> [Rucksacks]
toRucksack (s1:s2:s3:ss) = (s1,s2,s3):toRucksack ss
toRucksack ss = []

findDuplicate :: Rucksacks -> Char
findDuplicate ((c:cs),s2,s3) = if elem c s2 && elem c s3 then c else findDuplicate (cs,s2,s3)
findDuplicate ([],_,_) = error "no duplicate found"

scoreDuplicate :: Char -> Int
scoreDuplicate c = score c scores
    where
        score c1 ((c2,i):ss) = if c1 == c2 then i else score c1 ss

scores :: [(Char,Int)]
scores = zip ['a'..'z'] [1..26] ++ zip ['A'..'Z'] [27..52]