#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List.Split
import Data.List

main :: IO ()
main = interact $ show . sum . take 3 . reverse . sort . map sum . map (map (read :: String -> Int)) . map lines . splitOn "\n\n"