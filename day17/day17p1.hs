#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
import Debug.Trace
type Cave = [[Bool]]
type Rock = [[Bool]]
data Jets = JetLeft | JetRight deriving (Show,Eq)

main :: IO ()
main = interact $ show . game . cycle . parse . head . lines

game :: [Jets] -> Int
game jets = maxHeight . init . snd $ foldl fall startState rockList
    where
        rockList = take 2022 $ cycle rocks
        maxHeight :: Cave -> Int
        maxHeight bs = length bs - length (takeWhile (all not) bs)
        fall :: (Int, Cave) -> Rock -> (Int, Cave)
        fall (jp,cave) r = step jp (pad cave) r (1 - length r)
        step :: Int -> Cave -> Rock -> Int -> (Int,Cave)
        step jp cave rock offset =
            let rock' = jet (jets!!jp) cave rock offset
            in --trace (draw $ placeRock cave rock' offset) $
                if atBottom cave rock' offset
                then (jp+1, placeRock cave rock' offset)
                else step (jp + 1) cave rock' (offset + 1)
        atBottom :: Cave -> Rock -> Int -> Bool
        atBottom c r o = clash c r (o + 1)
        placeRock :: Cave -> Rock -> Int -> Cave
        placeRock cave rock offset =
            [   [ if rocky `elem` [0..(length rock -1)]
                  then cave!!y!!x || rock!!rocky!!x
                  else cave!!y!!x
                | x <- [0..6]
                ]
            | y <- [0..(length cave - 1)]
            , let rocky = y - offset
            ]
        pad :: Cave -> Cave
        pad cave = let deltay = 4 - ((length cave) - (maxHeight cave))  in (take (deltay) $ repeat emptyRow) ++ cave

jet :: Jets -> Cave -> Rock -> Int -> Rock
jet JetLeft cave rock offset = let rock' = if any head rock then rock else map (\r -> drop 1 r ++ [False]) rock
                               in if clash cave rock' offset
                                  then rock
                                  else rock'
jet JetRight cave rock offset = let rock' = if any last rock then rock else map (\r -> [False] ++ take 6 r) rock
                                in if clash cave rock' offset
                                   then rock
                                   else rock'

clash :: Cave -> Rock -> Int -> Bool
clash cave rock offset =
    let miny = max 0 (offset-(length rock -1))
        maxy = min offset (length cave - 1)
    in any id [
        any id [ if rocky `elem` [0..(length rock - 1)]
                 then cave!!y!!x && rock!!rocky!!x
                 else False
                | x <- [0..6]
                ]
            | y <- [0..(length cave - 1)] -- To be optimized
            , let rocky = y-offset
            ]

draw :: Cave -> String
draw c = (intercalate "\n" $ map (map toC) c) ++ "\n-------------------------\n"
    where
        toC True = '#'
        toC False = '.'
parse :: String -> [Jets]
parse = map toJet
    where
        toJet :: Char -> Jets
        toJet '<' = JetLeft
        toJet '>' = JetRight
        toJet  x  = error $ "Cannot parse " ++ [x]

startState = (0,startCave)

startCave :: Cave
startCave = [take 7 $ repeat True]
emptyRow :: [Bool]
emptyRow = take 7 $ repeat False

rocks :: [Rock]
rocks =
    let f = False
        t = True
    in  [
            [
                [f,f,t,t,t,t,f]
            ],
            [
                [f,f,f,t,f,f,f],
                [f,f,t,t,t,f,f],
                [f,f,f,t,f,f,f]
            ],
            [
                [f,f,f,f,t,f,f],
                [f,f,f,f,t,f,f],
                [f,f,t,t,t,f,f]
            ],
            [
                [f,f,t,f,f,f,f],
                [f,f,t,f,f,f,f],
                [f,f,t,f,f,f,f],
                [f,f,t,f,f,f,f]
            ],
            [
                [f,f,t,t,f,f,f],
                [f,f,t,t,f,f,f]
            ]
        ]