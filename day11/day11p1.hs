#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
data Monkey = Monkey { ident :: Int
                     , items :: [] Int
                     , operation :: Int -> Int
                     , test :: Int -> Bool
                     , ifTrue :: Int
                     , ifFalse :: Int
                     , inspections :: Int}

main :: IO ()
main = putStrLn . show . score . map inspections . (!!20) $ iterate turns ma

score :: [Int] -> Int
score is = (\(a:b:_) -> a * b) . reverse . sort $ is

turns :: [Monkey] -> [Monkey]
turns ms = foldl turn ms [0..(length ms - 1)]

turn :: [Monkey] -> Int -> [Monkey]
turn ms i = let monkey = ms !! i
                changes = [(if (test monkey) worry then ifTrue monkey else ifFalse monkey, worry) | item <- items monkey, let worry = ((operation monkey) item) `div` 3]
                numInspections = length changes
            in applyChanges changes . emptyList i . updateNumInsp i numInspections $ ms

applyChanges :: [(Int,Int)] -> [Monkey] -> [Monkey]
applyChanges [] ms = ms
applyChanges ((i,worry):rest) ms =
    let updated = (\x -> x { items = items x ++ [worry]}) (ms !! i)
        ms' = take i ms ++ (updated:drop (i+1) ms)
    in applyChanges rest ms'

emptyList :: Int -> [Monkey] -> [Monkey]
emptyList i ms =
    let emptied = (\x -> x { items = [] }) $ (ms !! i)
    in (take i ms) ++ [emptied] ++ (drop (i+1) ms)

updateNumInsp :: Int -> Int -> [Monkey] -> [Monkey]
updateNumInsp i n ms =
    let updated = (\x -> x { inspections = inspections x + n }) $ (ms !! i)
    in (take i ms) ++ [updated] ++ (drop (i+1) ms)

ma :: [] Monkey
ma = [ Monkey {ident = 0, items = [98,89,52], operation = (\x -> x * 2), test = (\x -> x `mod` 5 == 0), ifTrue = 6, ifFalse = 1, inspections = 0}
     , Monkey {ident = 1, items = [57, 95, 80, 92, 57, 78], operation = (\x -> x * 13), test = (\x -> x `mod` 2 == 0), ifTrue = 2, ifFalse = 6, inspections = 0}
     , Monkey {ident = 2, items = [82, 74, 97, 75, 51, 92, 83], operation = (\x -> x + 5), test = (\x -> x `mod` 19 == 0), ifTrue = 7, ifFalse = 5, inspections = 0}
     , Monkey {ident = 3, items = [97, 88, 51, 68, 76], operation = (\x -> x + 6), test = (\x -> x `mod` 7 == 0), ifTrue = 0, ifFalse = 4, inspections = 0}
     , Monkey {ident = 4, items = [63], operation = (\x -> x + 1), test = (\x -> x `mod` 17 == 0), ifTrue = 0, ifFalse = 1, inspections = 0}
     , Monkey {ident = 5, items = [94, 91, 51, 63], operation = (\x -> x + 4), test = (\x -> x `mod` 13 == 0), ifTrue = 4, ifFalse = 3, inspections = 0}
     , Monkey {ident = 6, items = [61, 54, 94, 71, 74, 68, 98, 83], operation = (\x -> x + 2), test = (\x -> x `mod` 3 == 0), ifTrue = 2, ifFalse = 7, inspections = 0}
     , Monkey {ident = 7, items = [90,56], operation = (\x -> x * x), test = (\x -> x `mod` 11 == 0), ifTrue = 3, ifFalse = 5, inspections = 0}
     ]

mt :: [Monkey]
mt = [ Monkey {ident = 0, items = [79, 98], operation = (\x -> x * 19), test = (\x -> x `mod` 23 == 0), ifTrue = 2, ifFalse = 3, inspections = 0}
     , Monkey {ident = 1, items = [54, 65, 75, 74], operation = (\x -> x + 6), test = (\x -> x `mod` 19 == 0), ifTrue = 2, ifFalse = 0, inspections = 0}
     , Monkey {ident = 2, items = [79, 60, 97], operation = (\x -> x * x), test = (\x -> x `mod` 13 == 0), ifTrue = 1, ifFalse = 3, inspections = 0}
     , Monkey {ident = 3, items = [74], operation = (\x -> x + 3), test = (\x -> x `mod` 17 == 0), ifTrue = 0, ifFalse = 1, inspections = 0}
     ]