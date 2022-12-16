#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
import Algorithm.Search
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace
data Valve = Valve (String,Int,[String]) deriving (Show, Eq)
data Link = Link (String,Int,Int,String) deriving (Show, Eq) -- (Start,Flow,Dist,End)

toLinks :: [Valve] -> [Link]
toLinks all = toLinks' all
    where
        nonZeros = filter (not . zeroFlow) all
        toLinks' (v:vs) = (concat . map (shortestPath v all) $ filter (not . sameName v) nonZeros) ++ toLinks' vs
        toLinks' [] = []

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
        Just (dist,steps) -> [Link (s1,f1,dist,s2)]
        Nothing -> []

zeroFlow :: Valve -> Bool
zeroFlow (Valve (_,i,_)) = i == 0

sameName :: Valve -> Valve -> Bool
sameName (Valve (a,_,_)) (Valve (b,_,_)) = a == b

main :: IO ()
main = interact $ show . toLinks . map parseValve . lines

solve :: [Valve] -> Maybe (Int, [(Int, Int, Int, M.Map String Bool, String)])
solve vs =
    let start = (30, 0, 0, setClosed vs,"AA")
    in dijkstra next cost end start
        where
            next state = openValve state ++ moveDirectly state
            cost (i1,f1,d1,_,_) (i2,f2,d2,_,_) = 10000-f2
            end (i,_,_,_,_) = i == 0
            setClosed [                  ] = M.empty
            setClosed ((Valve (s,_,_)):rest) = M.insert s False $ setClosed rest
            openValve (i,f,d,valveStatus, pos) = if newFlow 0 0 pos == 0 || (fromJust $ M.lookup pos valveStatus) then [] else [(i-1, newFlow f i pos, d-(newFlow f i pos), M.insert pos True valveStatus, pos)]
            moveDirectly (i,f,d,valveStatus,pos) = let (Valve (_,_,ss)) = getValve pos in map (\s -> (i-1, f, d-f, valveStatus,s)) ss
            newFlow f i pos = let (Valve (_,nf,ss)) = getValve pos in f + nf
            getValve pos = getValve' vs pos
            getValve' [] _ = error "Couldn't find valve"
            getValve' (v@(Valve (es,f,ss)):rest) cs = if es == cs then v else getValve' rest cs

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
