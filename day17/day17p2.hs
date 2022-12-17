#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
import Debug.Trace
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as C8
type Cave = [[Bool]]
type Rock = [[Bool]]
type Log  = [(Int,String)]
data Jets = JetLeft | JetRight deriving (Show,Eq)
-- Manual steps:
-- Get periodicity from log:
--   Get modulo to understand startpoint. (niter - startiter) / period * heightPerPeriod + startheight
--   Block 2800 -> Height 4553, Block 4510 -> Height 7000
--   Answer: (1000000000000-2800)/1710*2647 + 4353

main :: IO ()
main = interact $ show . game . map toJet . head . lines

game :: [Jets] -> Int
game jets = maxHeight . init . (\(_,a,_,_) -> a ) $ foldl fall startState rockList
    where
        rockList = take 8000 $ cycle rocks
        maxHeight :: Cave -> Int
        maxHeight bs = length bs - length (takeWhile (all not) bs)
        fall :: (Int, Cave, Int, Log) -> Rock -> (Int, Cave, Int, Log)
        fall (jp,cave,iter,log) r = step jp (pad cave) r (1 - length r) iter log
        step :: Int -> Cave -> Rock -> Int -> Int -> Log -> (Int, Cave, Int, Log)
        step jp cave rock offset iter log =
            let rock' = jet (jets!!jp) cave rock offset
                jp' = (jp + 1) `mod` (length jets)
            in  if atBottom cave rock' offset
                then let cave' = placeRock cave rock' offset
                         point = (jp, toChks $ take 30 cave')
                     in if point `elem` log || iter `mod` 100 == 0
                        then trace (show (iter+1) ++ ": " ++ show (maxHeight cave' - 1) ++ " " ++ show point) $ (jp', cave', iter +1, point:log)
                        else (jp', cave', iter +1, point:log)
                else step jp' cave rock' (offset + 1) iter log
        atBottom :: Cave -> Rock -> Int -> Bool
        atBottom c r o = clash c r (o + 1)
        placeRock :: Cave -> Rock -> Int -> Cave
        placeRock cave rock offset =
            [   [ if rocky `elem` [0..(length rock -1)]
                  then cave!!y!!x || rock!!rocky!!x
                  else cave!!y!!x
                | x <- [0..6] ]
            | y <- [0..(length cave - 1)]
            , let rocky = y - offset ]
        pad :: Cave -> Cave
        pad cave = let deltay = 4 - ((length cave) - (maxHeight cave))  in (take (deltay) $ repeat emptyRow) ++ cave

jet :: Jets -> Cave -> Rock -> Int -> Rock
jet JetLeft cave rock offset = let rock' = if any head rock then rock else map (\r -> drop 1 r ++ [False]) rock
                               in if clash cave rock' offset then rock else rock'
jet JetRight cave rock offset = let rock' = if any last rock then rock else map (\r -> [False] ++ take 6 r) rock
                                in if clash cave rock' offset then rock else rock'

clash :: Cave -> Rock -> Int -> Bool
clash cave rock offset =
    any id [
            any id [ if rocky `elem` [0..(length rock - 1)]
                     then cave!!y!!x && rock!!rocky!!x
                     else False
                   | x <- [0..6] ]
           | y <- [0..(length cave - 1)] -- To be optimized
           , let rocky = y-offset ]

toChks :: Cave -> String
toChks = show . md5 . C8.pack . draw

draw :: Cave -> String
draw c = (intercalate "\n" $ map (map toC) c) ++ "\n-------------------------\n"
toC True = '#'
toC False = '.'

toJet :: Char -> Jets
toJet '<' = JetLeft
toJet '>' = JetRight
toJet  x  = error $ "Cannot parse " ++ [x]

startState = (0,startCave,0,[])
startCave = [take 7 $ repeat True]
emptyRow = take 7 $ repeat False

rocks :: [Rock]
rocks = let f = False
            t = True
        in  [[[f,f,t,t,t,t,f]]
            ,[[f,f,f,t,f,f,f],[f,f,t,t,t,f,f],[f,f,f,t,f,f,f]]
            ,[[f,f,f,f,t,f,f],[f,f,f,f,t,f,f],[f,f,t,t,t,f,f]]
            ,[[f,f,t,f,f,f,f],[f,f,t,f,f,f,f],[f,f,t,f,f,f,f],[f,f,t,f,f,f,f]]
            ,[[f,f,t,t,f,f,f],[f,f,t,t,f,f,f]]]