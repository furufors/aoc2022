#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
main :: IO ()
main = interact $ show . indexOfMarker 0 . head . lines

indexOfMarker :: Int -> String -> Int
indexOfMarker i ss = let chars =  [ss!!k | j <- [0..3], let k = i + j]
                         uniq = nub $ sort chars
                     in if length uniq == 4 then i + 4 else indexOfMarker (i+1) ss