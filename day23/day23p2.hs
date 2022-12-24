#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List
data Status = Free | Taken deriving (Show, Eq)
type Point = (Int, Int)
data Elf = Elf (Point, [Point]) deriving (Show, Eq, Ord)
type Field = Map Point Status
type Suggestions = Map Point [Point] -- To [From]

main :: IO ()
main = interact $ show . converge step . (\x -> (0,x)) . parse . lines

converge f a = let a' = f a in if snd a == snd a' then fst a' else converge f a'

step :: (Int,Field) -> (Int,Field)
step (i,m) =
    let proposals = foldl elfProposal M.empty (elfPos m)
        elfProposal :: Suggestions -> Point -> Suggestions
        elfProposal s (x,y) =
            let checker xr yr = all (not . (==(Just Taken))) [M.lookup (a,b) m | dy <- yr, dx <- xr, let a = x + dx, let b = y + dy]
                north = checker [-1,0,1] [-1]
                south = checker [-1,0,1] [1]
                west = checker [-1] [-1,0,1]
                east = checker [1] [-1,0,1]
                postSuggestion p (Nothing) = Just $ p:[]
                postSuggestion p (Just ps) = Just $ p:ps
            in case i `mod` 4 of
                0 -> case (north, south, west, east) of
                        (True,True,True,True) -> M.alter (postSuggestion (x,y)) (x,y) s
                        (True,_   ,_   ,_   ) -> M.alter (postSuggestion (x,y)) (x,y-1) s
                        (_   ,True,_   ,_   ) -> M.alter (postSuggestion (x,y)) (x,y+1) s
                        (_   ,_   ,True,_   ) -> M.alter (postSuggestion (x,y)) (x-1,y) s
                        (_   ,_   ,_   ,True) -> M.alter (postSuggestion (x,y)) (x+1,y) s
                        (_   ,_   ,_   ,_   ) -> M.alter (postSuggestion (x,y)) (x,y) s
                1 -> case (north, south, west, east) of
                        (True,True,True,True) -> M.alter (postSuggestion (x,y)) (x,y) s
                        (_   ,True,_   ,_   ) -> M.alter (postSuggestion (x,y)) (x,y+1) s
                        (_   ,_   ,True,_   ) -> M.alter (postSuggestion (x,y)) (x-1,y) s
                        (_   ,_   ,_   ,True) -> M.alter (postSuggestion (x,y)) (x+1,y) s
                        (True,_   ,_   ,_   ) -> M.alter (postSuggestion (x,y)) (x,y-1) s
                        (_   ,_   ,_   ,_   ) -> M.alter (postSuggestion (x,y)) (x,y) s
                2 -> case (north, south, west, east) of
                        (True,True,True,True) -> M.alter (postSuggestion (x,y)) (x,y) s
                        (_   ,_   ,True,_   ) -> M.alter (postSuggestion (x,y)) (x-1,y) s
                        (_   ,_   ,_   ,True) -> M.alter (postSuggestion (x,y)) (x+1,y) s
                        (True,_   ,_   ,_   ) -> M.alter (postSuggestion (x,y)) (x,y-1) s
                        (_   ,True,_   ,_   ) -> M.alter (postSuggestion (x,y)) (x,y+1) s
                        (_   ,_   ,_   ,_   ) -> M.alter (postSuggestion (x,y)) (x,y) s
                3 -> case (north, south, west, east) of
                        (True,True,True,True) -> M.alter (postSuggestion (x,y)) (x,y) s
                        (_   ,_   ,_   ,True) -> M.alter (postSuggestion (x,y)) (x+1,y) s
                        (True,_   ,_   ,_   ) -> M.alter (postSuggestion (x,y)) (x,y-1) s
                        (_   ,True,_   ,_   ) -> M.alter (postSuggestion (x,y)) (x,y+1) s
                        (_   ,_   ,True,_   ) -> M.alter (postSuggestion (x,y)) (x-1,y) s
                        (_   ,_   ,_   ,_   ) -> M.alter (postSuggestion (x,y)) (x,y) s
        nonClashingProposals :: [(Point,Point)]
        nonClashingProposals = map (\(k,v) -> (head v, k)) . filter (\(k,v) -> length v == 1) $ M.assocs proposals
        move :: [(Point,Point)] -> Field
        move ps = let toFree = map fst ps
                      toTaken = map snd ps
                  in foldl (\m p -> M.insert p Taken m) (foldl (\m p -> M.insert p Free m) m toFree) toTaken
        m' = move nonClashingProposals
    in (i+1, m')

draw :: Field -> String
draw m =
    let elfs = elfPos m
        elfCount = length elfs
        maxx = maximum $ map fst elfs
        minx = minimum $ map fst elfs
        maxy = maximum $ map snd elfs
        miny = minimum $ map snd elfs
        toChar Nothing = '.'
        toChar (Just Free) = '.'
        toChar (Just Taken) = '#'
    in (intercalate "\n" $ [[toChar (M.lookup (x,y) m) | x <-[minx..maxx] ] | y <- [miny..maxy] ]) ++ "\n\n"

parse :: [String] -> Field
parse ss = let indexed = zip (map (zip [0..]) ss) [0..]
               fun (a,y) = map (\(x,e) -> (e,(x,y))) a
               elems = concat $ map fun indexed
               toStatus '#' = Taken
               toStatus  x  = Free
           in foldl (\m (e,p) -> M.insert p (toStatus e) m) M.empty elems

elfPos :: Field -> [Point]
elfPos = map fst . filter (\(k,v) -> v == Taken) . M.assocs

containingBox :: Field -> Int
containingBox m = let elfs = elfPos m
                      elfCount = length elfs
                      maxx = maximum $ map fst elfs
                      minx = minimum $ map fst elfs
                      maxy = maximum $ map snd elfs
                      miny = minimum $ map snd elfs
                  in trace (show elfCount) $ (maxx - minx + 1) * (maxy - miny + 1) - elfCount