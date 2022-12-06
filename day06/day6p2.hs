#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
main :: IO ()
main = interact $ show . indexOfMarker 0 . head . lines

indexOfMarker :: Int -> String -> Int
indexOfMarker i ss = let chars =  [ss!!k | j <- [0..13], let k = i + j]
                         uniq = nub $ sort chars
                     in if length uniq == 14 then i + 14 else indexOfMarker (i+1) ss