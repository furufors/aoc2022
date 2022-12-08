#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Char
main :: IO ()
main = interact $ show . countVisible . map (map digitToInt) . lines

countVisible :: [[Int]] -> Int
countVisible ss = sum . map sum $
    [
        [if allAbove || allUnder || allLeft || allRight then 1 else 0
        | let xmax = length (head ss) -1,
          x <- [0..xmax],
          let allAbove = x == 0    || all (<ss!!y!!x) [ss!!y!!xvar | xvar <- [0..(x-1)]],
          let allUnder = x == xmax || all (<ss!!y!!x) [ss!!y!!xvar | xvar <- [(x+1)..xmax]],
          let allLeft  = y == 0    || all (<ss!!y!!x) [ss!!yvar!!x | yvar <- [0..(y-1)]],
          let allRight = y == ymax || all (<ss!!y!!x) [ss!!yvar!!x | yvar <- [(y+1)..ymax]]
        ]
    | let ymax = length ss - 1,
      y <- [0..ymax]
    ]