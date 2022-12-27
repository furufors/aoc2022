#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
{-# LANGUAGE BangPatterns #-}
import Text.Parsec
import Algorithm.Search
import Control.DeepSeq
import Control.Parallel
import Control.Parallel.Strategies
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List
import Data.Time.Clock
import Debug.Trace
type Valve = (String,Int,[String])
type Link = (String,Int,Int,Int,String) -- (Start,Flow,EFlow,Dist,End)
type Dist = M.Map String Int
type Flow = M.Map String Int
-- Starting at: cat .\input.txt | .\day16p1.exe +RTS -N3 -> 167.1150585s
-- Optimized to: cat .\input.txt | .\day16p1_opt.exe +RTS -N3 ->
-- Memoized steps: 31.4439468s
-- Parallelized comprehension -N3: 18.9486785s
-- Parallelized comprehension -N7: 11.6395611s

timeIt :: IO a -> IO a
timeIt ioa = do
    t1 <- getCurrentTime
    a <- ioa
    t2 <- getCurrentTime
    let t = diffUTCTime t2 t1
    putStrLn ("\n" ++ show t)
    return a

main :: IO ()
main = timeIt $ interact $ show . bruteforce . toMaps . toLinks . force . parMap rpar parseValve . lines

toMaps :: [Link] -> (Flow, Dist)
toMaps ls = force $ foldl' conv (M.empty, M.empty) ls
    where
        conv (fm,dm) (s,f,ef,d,e) = (M.insert e ef fm, M.insert (s++e) d dm)

toLinks :: [Valve] -> [Link]
toLinks all = filter (remzero) $ toLinks' all
    where
        nonZeros = filter (not . zeroFlow) all
        toLinks' (v:vs) = (force . concat . parMap rpar (shortestPath v all) $ filter (not . sameName v) nonZeros) ++ toLinks' vs
        toLinks' [] = []
        zeroFlow :: Valve -> Bool
        zeroFlow (_,i,_) = i == 0
        sameName :: Valve -> Valve -> Bool
        sameName (a,_,_) (b,_,_) = a == b
        remzero ("AA",_,_,_,_) = True
        remzero (_,0,_,_,_) = False
        remzero a = True

shortestPath :: Valve -> [Valve] -> Valve -> [Link]
shortestPath (s1,f1,ss1) vs (s2,f2,ss2) =
    let sol = dijkstra next cost end start
        next s = let (_,_,ss) = getValve s in ss
        cost _ _= 1
        end pos = pos == s2
        start = s1
        getValve pos = getValve' vs pos
        getValve' [] pos = error $ "Couldn't find valve: " ++ pos ++ (show vs)
        getValve' (v@(es,f,ss):rest) cs = if es == cs then v else getValve' rest cs
    in case sol of
        Just (dist,steps) -> [(s1,f1,f2,dist,s2)]
        Nothing -> []

bruteforce :: (Flow, Dist) -> Int
bruteforce (flowMap, distMap) =
    let names = filter (/="AA") . nub $ M.keys flowMap
        --depth = (\x -> x-1) . length . takeWhile (<=30) . scanl1 (+) . sort $ M.elems distMap
        step :: Int -> (Int, [String]) -> Int
        step 0 _ = 0
        step n (i,s) =
            let nexts = (names \\ s)
                !runs = parMap rpar (\next -> (run i (last s) next,(s ++ [next]))) nexts
            in if null nexts
               then 0
               else maximum $ parMap rpar (\((flow, i'), path) -> flow + step (n-1) (i', path)) $ runs
        run :: Int -> String -> String -> (Int, Int)
        run i a b | i > 0 = case (M.lookup (a++b) distMap, M.lookup b flowMap) of
            (Just d, Just f) -> let i' = i-d-1 in ((max 0 i')*f, i')
            otherwise -> error $ a++b
        run _ _ _ = (0,0)
    in step 8 (30,["AA"])

parseValve :: String -> Valve
parseValve input = case parse (try valve) "parsein" input of
    Left err -> error $ show err
    Right a -> a

valve :: Parsec String () Valve
valve = do
    _  <- string "Valve "
    i  <- many1 upper
    _  <- string " has flow rate="
    r  <- read <$> many1 digit
    _  <- string "; tunnel"
    _  <- string "s lead to valves " <|> string " leads to valve "
    vs <- (many1 upper) `sepBy` (string ", ")
    return $ (i,r,vs)