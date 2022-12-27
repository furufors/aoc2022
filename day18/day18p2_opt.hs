#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
import Data.Set (Set)
import Data.List
import Control.Parallel
import Control.Parallel.Strategies
import qualified Data.Set as Set
import Data.Time.Clock
type Rock = (Int,Int,Int)

timeIt :: IO a -> IO a
timeIt ioa = do
    t1 <- getCurrentTime
    a <- ioa
    t2 <- getCurrentTime
    let t = diffUTCTime t2 t1
    putStrLn ("\n" ++ show t)
    return a

main :: IO ()
main = timeIt $ interact $ show . compute . parMap rpar parseRock . lines

compute rocks =
    let roxette = Set.fromList rocks
        minmax acc = let ps = parMap rpar acc rocks in (minimum ps - 1, maximum ps + 1)
        (minx,maxx) = minmax (\(x,_,_) -> x)
        (miny,maxy) = minmax (\(_,y,_) -> y)
        (minz,maxz) = minmax (\(_,_,z) -> z)
        neighbours (x,y,z) = filter insideBounds [(x-1,y,z),(x+1,y,z),(x,y-1,z),(x,y+1,z),(x,y,z-1),(x,y,z+1)]
        insideBounds (x,y,z) = minx <= x && x <= maxx && miny <= y && y <= maxy && minz <= z && z <= maxz
        visit :: [Rock] -> Set Rock -> Int -> Int
        visit [] visited sideSeen = sideSeen
        visit ns visited sideSeen =
            let updates = concat . parMap rpar (\n -> map update $ Set.toList $ (Set.fromList $ neighbours n) Set.\\ visited) $ ns
                newNodes = nub . concat $ map fst updates
                newSides = sum $ map snd updates
                update new = if Set.member new roxette then ([], 1) else ([new],0)
                visited' = Set.union (Set.fromList newNodes) visited
            in updates `seq` ((newNodes `seq` visited') `par` newSides) `seq` visit (newNodes) visited' (sideSeen + newSides)
    in roxette `par` minx `par` maxx `par` miny `par` maxy `par` minz `par`maxz `seq` visit [(minx,miny,minz)] (Set.singleton (minx,miny,minz)) 0

parseRock :: String -> Rock
parseRock input = let rock = read <$> many1 digit >>= \x -> string "," >> read <$> many1 digit >>= \y -> string "," >>  read <$> many1 digit >>= \z -> return (x,y,z)
                  in case parse rock "parsein" input of
                        Left err -> error $ show err
                        Right a -> a