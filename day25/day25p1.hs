#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = interact $ show . toSNAFU . sum . map (toDec 1 . reverse) . lines

toDec :: Int -> [Char] -> Int
toDec _ [      ] = 0
toDec f ('=':as) = -2 * f + toDec (5*f) as
toDec f ('-':as) = -1 * f + toDec (5*f) as
toDec f ('0':as) =          toDec (5*f) as
toDec f ('1':as) =  1 * f + toDec (5*f) as
toDec f ('2':as) =  2 * f + toDec (5*f) as

toSNAFU :: Int -> String
toSNAFU 0 = ""
toSNAFU n = let (d,r) = divMod n 5 in case r of
    0 -> toSNAFU d ++ "0"
    1 -> toSNAFU d ++ "1"
    2 -> toSNAFU d ++ "2"
    3 -> toSNAFU (d+1) ++ "="
    4 -> toSNAFU (d+1) ++ "-"