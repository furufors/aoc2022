#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
import Algorithm.Search
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Debug.Trace
data Valve = Valve (String,Int,[String]) deriving (Show, Eq)
data Link = Link (String,Int,Int,Int,String) deriving (Show, Eq) -- (Start,Flow,EFlow,Dist,End)
type Dist = M.Map String Int
type Flow = M.Map String Int

toMaps :: [Link] -> (Flow, Dist)
toMaps ls = foldl (conv) (M.empty, M.empty) ls
    where
        conv (fm,dm) (Link (s,f,ef,d,e)) = (M.insert e ef fm, M.insert (s++e) d dm)

toLinks :: [Valve] -> [Link]
toLinks all = filter (remzero) $ toLinks' all
    where
        nonZeros = filter (not . zeroFlow) all
        toLinks' (v:vs) = (concat . map (shortestPath v all) $ filter (not . sameName v) nonZeros) ++ toLinks' vs
        toLinks' [] = []

remzero (Link ("AA",_,_,_,_)) = True
remzero (Link (_,0,_,_,_)) = False
remzero a = True

shortestPath :: Valve -> [Valve] -> Valve -> [Link]
shortestPath (Valve (s1,f1,ss1)) vs (Valve (s2,f2,ss2)) =
    let sol = dijkstra next cost end start
        next s = let (Valve (_,_,ss)) = getValve s in ss
        cost _ _= 1
        end pos = pos == s2
        start = s1
        getValve pos = getValve' vs pos
        getValve' [] pos = error $ "Couldn't find valve: " ++ pos ++ (show vs)
        getValve' (v@(Valve (es,f,ss)):rest) cs = if es == cs then v else getValve' rest cs
    in case sol of
        Just (dist,steps) -> [Link (s1,f1,f2,dist,s2)]
        Nothing -> []

zeroFlow :: Valve -> Bool
zeroFlow (Valve (_,i,_)) = i == 0

sameName :: Valve -> Valve -> Bool
sameName (Valve (a,_,_)) (Valve (b,_,_)) = a == b

main :: IO ()
main = interact $ show . bruteforce . toMaps . toLinks . map parseValve . lines

bruteforce :: (Flow, Dist) -> Int
bruteforce (flowMap, distMap) = trace (show names) $ maximum [run ("AA":order) | ns <- subsetLists, order <- permutations ns]
    where
        subsetLists = subsets 8 names
        names = filter (/="AA") . nub $ M.keys flowMap
        run :: [String] -> Int
        run order = run' 30 order
        run' i (a:b:rest) | i > 0 = case (M.lookup (a++b) distMap, M.lookup b flowMap) of
            (Just d, Just f) -> let i' = i-d-1 in (max 0 i')*f + (run' i' (b:rest))
            otherwise -> error $ a++b
        run' _ _ = 0
        subsets 0 _ = [[]]
        subsets _ [] = []
        subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs

{- solve :: [Link] -> Maybe (Int, [(Int, Int, Int, M.Map String Bool, String)])
solve vs =
    let start = (30, 0, 0, setClosed vs,"AA")
        setClosed [                  ] = M.empty
        setClosed ((Link (s,_,_,_)):rest) = M.insert s False $ setClosed rest
    in dijkstra next cost end start
        where
            --heur (i,f,d,_,_) = -
            next state = openValve state ++ moveDirectly state -- ++ wait state
            wait (i,f,d,valveStatus, pos) = [(i+1,f,d-f,valveStatus, pos)]
            cost (i1,f1,d1,_,_) (i2,f2,d2,_,_) = -d2
            end (i,_,_,_,_) = i <= 0
            openValve (i,f,d,valveStatus, pos) = if newFlow 0 0 pos == 0 || (fromJust $ M.lookup pos valveStatus) then [] else [(i-1, newFlow f i pos, d + ((newFlow 0 0 pos) * (i-1)), M.insert pos True valveStatus, pos)]
            moveDirectly (i,f,d,valveStatus,pos) = let next = nub $ filter (\p -> isClosed valveStatus p && from pos p) vs in map (\(Link (_,_,dist,s)) -> (i-dist, f, d, valveStatus,s)) $ next
            from s l@(Link (a,_,_,_)) = s == a
            newFlow f i pos = let (Link (_,nf,_,_)) = getValve pos in f + nf
            isClosed a (Link (b,_,_,_)) = not . fromJust $ M.lookup b a
            getValve pos = getValve' vs pos
            getValve' [] cs = error $ "Couldn't find valve: " ++ cs ++ (show vs)
            getValve' (v@(Link (es,f,dist,ss)):rest) cs = if es == cs then v else getValve' rest cs -}

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
    return $ Valve (i,r,vs)
-- Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
