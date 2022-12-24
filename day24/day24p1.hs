#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Set (Set)
import qualified Data.Set as S
import Data.List
import Control.Parallel
import Control.Parallel.Strategies
import Debug.Trace
type Pos = (Int, Int)
data Blizzard = U Pos | L Pos | D Pos | R Pos deriving (Show, Eq, Ord)
type GamePlan = Set Pos

main :: IO ()
main = interact $ show . run . parse . lines

parse :: [String] -> (Int, Int, [Blizzard])
parse ss = (length (ss!!0) - 2 , length ss - 2, bs)
    where
        iyx = concat $ map (\(y,ls) -> map (\l -> (y,l)) ls) . zip [0..] . init . tail $ map (zip [0..] . init . tail) ss
        bs = concat $ map (\(y,(x,c)) -> parseChar (x,y,c)) iyx
parseChar (x,y,'^') = [U (x,y)]
parseChar (x,y,'<') = [L (x,y)]
parseChar (x,y,'v') = [D (x,y)]
parseChar (x,y,'>') = [R (x,y)]
parseChar _ = []

run :: (Int, Int, [Blizzard]) -> Int
run (w, h, bs) =
    let memoizedBs :: Int -> Set Blizzard
        memoizedBs = (map blizzard [0 ..] !!)
            where
                blizzard 0 = foldl (flip S.insert) S.empty bs
                blizzard n = nextBlizzard (memoizedBs (n-1))
                nextBlizzard x = foldl (flip S.insert) S.empty . map move $ S.elems x
                move (U (x,y)) = U (x, (y-1) `mod` h)
                move (L (x,y)) = L ((x-1) `mod` w, y)
                move (D (x,y)) = D (x, (y+1) `mod` h)
                move (R (x,y)) = R ((x+1) `mod` w, y)
        insertBlizzard s (U p) = S.insert p s
        insertBlizzard s (L p) = S.insert p s
        insertBlizzard s (D p) = S.insert p s
        insertBlizzard s (R p) = S.insert p s
        goal = (w-1,h)
        start = (0,-1)
        rounds i ps =
            let gp = foldl insertBlizzard S.empty (S.elems $ memoizedBs (i+1))
                step :: Pos -> [Pos]
                step (x,y) =
                    concat [ if S.member (a,b) gp  then [] else [(a,b)]
                           | (dx,dy) <- [(1,0),(0,1),(-1,0),(0,-1),(0,0)]
                           , let a = x + dx, let b = y + dy
                           , (0 <= a && a < w && 0 <= b && b < h) || (a,b) == goal || (a,b) == start
                           ]
                next = nub . concat $ parMap rpar step ps
            in if goal `elem` next
               then i + 1
               else if null next
                    then error "Run out of candidates"
                    else rounds (i+1) next
    in rounds 0 [start]
