#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Char
main :: IO ()
main = interact $ show . maximum . map maximum . countVisible . map (map digitToInt) . lines

countVisible :: [[Int]] -> [[Int]]
countVisible ss =
    [
        [allAbove * allUnder * allLeft * allRight
        | let xmax = length (head ss) -1,
          x <- [0..xmax],
          let val = ss!!y!!x,
          let left = reverse [ss!!y!!xvar | xvar <- [0..(x-1)]],
          let right = [ss!!y!!xvar | xvar <- [(x+1)..xmax]],
          let above = reverse [ss!!yvar!!x | yvar <- [0..(y-1)]],
          let under = [ss!!yvar!!x | yvar <- [(y+1)..ymax]],
          let allLeft  = if x == 0    then 0 else numberOfIncreamenting val left,
          let allRight = if x == xmax then 0 else numberOfIncreamenting val right,
          let allAbove = if y == 0    then 0 else numberOfIncreamenting val above,
          let allUnder = if y == ymax then 0 else numberOfIncreamenting val under
        ]
    | let ymax = length ss - 1,
      y <- [0..ymax]
    ]

numberOfIncreamenting :: Int -> [Int] -> Int
numberOfIncreamenting v (a:ss) = if a < v then 1 + (numberOfIncreamenting (max a v) (ss)) else if a >= v then 1 else 0
numberOfIncreamenting _ [    ] = 0
