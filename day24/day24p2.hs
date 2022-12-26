#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
{-# LANGUAGE BangPatterns #-}
import Control.DeepSeq
import Data.Set (Set)
import qualified Data.Set as S
import Data.List
import Control.Parallel
import Control.Parallel.Strategies
import Debug.Trace
import Data.Time.Clock
import Text.Printf (printf)
type Pos = (Int, Int)
data Blizzard = U Pos | L Pos | D Pos | R Pos deriving (Show, Eq, Ord)
type GamePlan = Set Pos

timeIt :: IO a -> IO a
timeIt ioa = do
    t1 <- getCurrentTime
    a <- ioa
    t2 <- getCurrentTime
    let t = diffUTCTime t2 t1
    putStrLn ("\n" ++ show t)
    return a

main :: IO ()
main = timeIt $ interact $ show . run . parse . lines

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
                blizzard 0 = foldl' (flip S.insert) S.empty bs
                blizzard n = nextBlizzard (memoizedBs (n-1))
                nextBlizzard x = foldl' (flip S.insert) S.empty . map move $ S.elems x
                move (U (x,y)) = U (x, (y-1) `mod` h)
                move (L (x,y)) = L ((x-1) `mod` w, y)
                move (D (x,y)) = D (x, (y+1) `mod` h)
                move (R (x,y)) = R ((x+1) `mod` w, y)
        memoizedGp = (map gamePlan [0..] !!)
            where
                gamePlan n = foldl' insertBlizzard S.empty (S.elems $ memoizedBs n)
                insertBlizzard s (U p) = S.insert p s
                insertBlizzard s (L p) = S.insert p s
                insertBlizzard s (D p) = S.insert p s
                insertBlizzard s (R p) = S.insert p s
        goal = (w-1,h)
        start = (0,-1)
        rounds s g i ps =
            let step :: Pos -> [Pos]
                step (x,y) = force $
                    concat [ if S.member (a,b) (memoizedGp (i + 1))  then [] else [(a,b)]
                           | (dx,dy) <- [(1,0),(0,1),(-1,0),(0,-1),(0,0)]
                           , let a = x + dx, let b = y + dy
                           , (0 <= a && a < w && 0 <= b && b < h) || (a,b) == s || (a,b) == g
                           ]
                next = nub . concat $ parMap rpar step ps
            in if g `elem` next
               then i + 1
               else if null next
                    then error "Run out of candidates"
                    else rounds s g (i+1) next
        minutesSg = rounds start goal 0 [start]
        minutesGs = rounds goal start (minutesSg-1) [goal]
        minutesSg2 = rounds start goal (minutesGs-1) [start]
    in memoizedGp 720 `par` trace (show (minutesSg-0) ++ " + " ++ show (minutesGs - minutesSg) ++ " + " ++ show (minutesSg2 - minutesGs)) $ minutesSg2
