#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
main :: IO ()
main = interact $ show . indexOfMarker 0 . head . lines

indexOfMarker :: Int -> String -> Int
indexOfMarker i ss = let chars =  [ss!!k | j <- [0..13], let k = i + j]
                         uniq = nub $ sort chars
                         lenUniq = length  uniq
                         len = length chars
                     in if lenUniq == 14 then i + 14 else indexOfMarker (i+1) ss