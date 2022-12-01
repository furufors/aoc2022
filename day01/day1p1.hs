#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
{-# LANGUAGE LambdaCase #-}
import Data.List.Split
main :: IO ()
main = interact $ show . maximum . map sum . map (map (read :: String -> Int)) . map lines . splitOn "\n\n"

--toMax :: [[String]] -> Int
--toMax = max . map sum .

{-
\case { '.' -> e1; ...; .^. -> eN }
main = interact $ show . sum . map (\case '(' -> 1; ')' -> -1; _ -> 0)
main = interact $ show . (+1) . length . takeWhile (>= 0) . scanl1 (+) . map (\case '(' -> 1; ')' -> -1; _ -> 0)
main = interact $ show . length . filter (id) . (\l -> zipWith (>) (drop 1 l) l) . map (\x -> read x :: Int) . lines

main = interact $ show . trues . increasing . map tripletsum . triplets . parsein
    where
        trues = length . filter id
        increasing l = zipWith (>) (drop 1 l) l
        tripletsum (a,b,c) = a + b + c
        triplets l = zip3 (drop 2 l) (drop 1 l) l
        parsein = map (\x -> read x :: Int) . lines
-}