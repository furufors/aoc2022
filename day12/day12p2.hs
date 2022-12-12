#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Char
import Algorithm.Search
import Data.Maybe
import Control.Parallel.Strategies
type State = (Int,Int)

main :: IO ()
main = interact $ show . fromJust . solve . parMap rpar (map $ (\x -> x-97) . ord) . lines

solve :: [[Int]] -> Maybe (Int, [State])
solve key = dijkstra next cost solved start
    where
        solved :: State -> Bool
        solved (x,y) = (x,y) `elem` [(sx,sy) | sx <- [0..(length (head key) -1)], sy <- [0..(length key -1)], key!!sy!!sx == ((\x -> x-97) $ ord 'a')]
        start :: State
        start = (43,20)
        cost :: State -> State -> Int
        cost _ _ = 1
        next :: State -> [State]
        next (x,y) = filter (valid (x,y)) $ [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
        valid :: State -> State -> Bool
        valid (_,_) (x,_) | x < 0 || x >= length (head key) = False
        valid (_,_) (_,y) | y < 0 || y >= length (key) = False
        valid (x,y) (nx,ny) = case (key!!y!!x , key!!ny!!nx ) of
            (-28,25) -> True -- | f == ord 'z' && t == ord 'E'
            (-28,_) -> False
            (f,t) -> t-f >= -1