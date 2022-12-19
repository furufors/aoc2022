#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
import Data.List
import Control.Parallel
import Control.Parallel.Strategies
import System.TimeIt
type Inventory = (Int {- Ore -}, Int {- Clay -}, Int {- Obsidian -}, Int {- Geode -})
type RunState = (Inventory {- Resources -}, Inventory {- Robots -})
type BluePrint = (Int {- Id -}, Inventory {- OreRobot-}, Inventory {- ClayRobot-}, Inventory {- ObsidianRobot-}, Inventory {- GeodeRobot-})

main :: IO ()
main = timeIt $ interact $ show . foldl1 (*) . parMap rpar run . map parseBluePrint . take 3 . lines

run :: BluePrint -> Int
run bp@(i,orePrice, claPrice, obsPrice, geoPrice) =
    let step :: Int -> BluePrint -> [RunState] -> Int
        step n bp ss =
            if n == 0
            then (maximum $ map (geo . fst) ss)
            else step (n-1) bp . prune . reverse . prune $ [ next | next <- concat (parMap rpar nextStates ss)]
        maxNecessaryOre = maximum (map ore [orePrice,claPrice,obsPrice, geoPrice])
        maxNecessaryCla = cla obsPrice
        maxNecessaryObs = obs geoPrice
        nextStates (resources, robots) =
            let resources' = resources `plus` robots
                -- Do not overspend on resources which cannot be consumed:
                oreCond = ore robots < maxNecessaryOre && allowed (resources `minus` orePrice)
                claCond = cla robots < maxNecessaryCla && allowed (resources `minus` claPrice)
                obsCond = obs robots < maxNecessaryObs && allowed (resources `minus` obsPrice)
                geoCond = allowed (resources `minus` geoPrice)
                bide = [(resources',robots)]
                buyOreRobot = if oreCond then [(resources' `minus` orePrice, robots `plus` (1,0,0,0))] else []
                buyClaRobot = if claCond then [(resources' `minus` claPrice, robots `plus` (0,1,0,0))] else []
                buyObsRobot = if obsCond then [(resources' `minus` obsPrice, robots `plus` (0,0,1,0))] else []
                buyGeoRobot = if geoCond then [(resources' `minus` geoPrice, robots `plus` (0,0,0,1))] else []
                buyingStates = buyOreRobot ++ buyClaRobot ++ buyObsRobot ++ buyGeoRobot
            in if geoCond
            then buyGeoRobot
            else if and [oreCond, claCond, obsCond]
                    then buyOreRobot ++ buyClaRobot ++ buyObsRobot
                    else buyingStates ++ bide
    in step 32 bp [((0,0,0,0),(1,0,0,0))]

ore (a,_,_,_) = a
cla (_,b,_,_) = b
obs (_,_,c,_) = c
geo (_,_,_,d) = d

prune :: [RunState] -> [RunState]
prune []     = []
prune (a:as) = a:prune (filter (not . \a' -> a' `bothWorse` a) as)
    where
        bothWorse (resourcesA,robotsA) (resourcesB,robotsB) =
            resourcesA `worse` resourcesB && robotsA `worse` robotsB

worse :: Inventory -> Inventory -> Bool
(a,b,c,d) `worse` (e,f,g,h) = a <= e && b <= f && c <= g && d <= h

minus :: Inventory -> Inventory -> Inventory
(a,b,c,d) `minus` (e,f,g,h) = (a-e,b-f,c-g,d-h)

plus :: Inventory -> Inventory -> Inventory
(a,b,c,d) `plus` (e,f,g,h) = (a+e,b+f,c+g,d+h)

allowed :: Inventory -> Bool
allowed (a,b,c,d) = a >= 0 && b >= 0 && c >= 0 && d >= 0

multiple :: Int -> Inventory -> Inventory
multiple x (a,b,c,d) = (x*a,x*b,x*c,x*d)

parseBluePrint :: String -> BluePrint
parseBluePrint input = case parse bluePrint "parsein" input of
    Left err -> error $ show err
    Right a -> a

-- Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 17 clay. Each geode robot costs 4 ore and 20 obsidian.
bluePrint :: Parsec String () BluePrint
bluePrint = do
    _  <- string "Blueprint "
    i1 <- read <$> many1 digit
    _  <- string ": Each ore robot costs "
    i2 <- read <$> many1 digit
    _  <- string " ore. Each clay robot costs "
    i3 <- read <$> many1 digit
    _  <- string " ore. Each obsidian robot costs "
    i4 <- read <$> many1 digit
    _  <- string " ore and "
    i5 <- read <$> many1 digit
    _  <- string " clay. Each geode robot costs "
    i6 <- read <$> many1 digit
    _  <- string " ore and "
    i7 <- read <$> many1 digit
    _  <- string " obsidian."
    return $ (i1,(i2,0,0,0),(i3,0,0,0),(i4,i5,0,0),(i6,0,i7,0))