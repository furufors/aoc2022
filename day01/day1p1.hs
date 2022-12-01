#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List.Split

main :: IO ()
main = interact $ show . maximum . map sum . map (map (read :: String -> Int)) . map lines . splitOn "\n\n"
