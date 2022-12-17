#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
type Cave = [[Bool]]
type Rock = [[Bool]]
data Jets = JetLeft | JetRight deriving (Show,Eq)

main :: IO ()
main = interact $ show . game . parse . head . lines

game :: [Jets] -> Int
game jets = maxHeight . snd $ foldl fall startState rockList
    where
        rockList = reverse $ take 2022 $ cycle rocks
        maxHeight :: Cave -> Int
        maxHeight bs = length bs - length (takeWhile (all not) bs)
        fall :: (Int, Cave) -> Rock -> (Int, Cave)
        fall (jp,cave) r = step jp (pad cave) rock 0
        step jp cave rock offset = if atBottom cave rock offset
                                   then (jp,cave)
                                   else step (jp + 1) (jet (jets!!jp) rock) (offset + 1)
        atBottom cave rock offset = any [any id [cave!!(y+offset)!!x && rock!!y!!x | x <- [0..6]] | y <- [0..(length rock - 1)]]
        pad cave = let deltay = length cave - maxHeight cave - 3 in take (deltay) emptyRow ++ cave

jet :: Jets -> Rock -> Rock
jet JetLeft  rock = if any head rock then rock else map (\r -> drop 1 r ++ [False]) rock
jet JetRight rock = if any last rock then rock else map (\r -> [False] ++ take 6 r) rock

parse :: String -> [Jets]
parse = map toJet
    where
        toJet :: Char -> Jets
        toJet '<' = JetLeft
        toJet '>' = JetRight
        toJet  x  = error $ "Cannot parse " ++ [x]

startState = (0,startCave)

startCave :: Cave
startCave = take 3 $ repeat emptyRow
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