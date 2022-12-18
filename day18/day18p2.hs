#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
import Data.Set (Set)
import qualified Data.Set as Set
import System.TimeIt
type Rock = (Int,Int,Int)

main :: IO ()
main = timeIt $ interact $ show . compute . map parseRock . lines

compute rocks =
    let minp = minimum (map (\(x,y,z) -> minimum [x,y,z]) rocks) - 1
        maxp = maximum (map (\(x,y,z) -> maximum [x,y,z]) rocks) + 1
        neighbours (x,y,z) = filter insideBounds [(x-1,y,z),(x+1,y,z),(x,y-1,z),(x,y+1,z),(x,y,z-1),(x,y,z+1)]
        insideBounds (x,y,z) = all (\p -> minp <= p && p <= maxp) [x,y,z]
        visit :: [Rock] -> Set Rock -> Int -> Int
        visit [] visited sideSeen = sideSeen
        visit (n:ns) visited sideSeen =
            let newNeighbours = (Set.fromList $ neighbours n) Set.\\ visited
                (ns', sideSeen', visited') = foldl update (ns,sideSeen,visited) newNeighbours
                update (ns, sideSeen, visited) new =
                    if new `elem` rocks
                    then (ns, sideSeen + 1, visited)
                    else (new:ns,sideSeen, Set.insert new visited)
            in visit ns' visited' sideSeen'
    in visit [(minp,minp,minp)] (Set.singleton (minp,minp,minp)) 0

parseRock :: String -> Rock
parseRock input = case parse rock "parsein" input of
    Left err -> error $ show err
    Right a -> a

rock = read <$> many1 digit >>= \x -> string "," >> read <$> many1 digit >>= \y -> string "," >>  read <$> many1 digit >>= \z -> return (x,y,z)