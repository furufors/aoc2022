#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
import Debug.Trace
import Data.List (intercalate)
type Rock = (Int,Int)
type Cave = [[Bool]]

main :: IO ()
main = interact $ show . run . startcave . normalize . map parseRock . lines

offset = 200

run :: (Int, (Int,Int), Int, Cave) -> Int
--run (s,lastx, iter, cave) | lastx < 0 || lastx >= (length (head cave)) = iter - 1
run (s,lastx, iter, cave) =
    let (cave',lastx') = sand (s,0) cave
    in if lastx == lastx' then trace (draw cave') $ iter - 1 else run (s, lastx', iter+1, cave')

draw :: Cave -> String
draw cave = intercalate "\n" $ map (map (\x -> if x then '#' else '.')) cave

sand :: (Int,Int) -> Cave -> (Cave,(Int,Int))
sand (x,y) cave | x<0 || x>=(length (head cave)) || y<0 || y>=(length cave) = (cave,(x,y))
sand (x,y) cave | cave!!y!!x == False = sand (x,y+1) cave
sand (x,y) cave = if x-1 < 0 || x+1 >= (length (head cave)) || y-1 < 0 || y >= (length cave)
                  then (cave,(x,y))
                  else case (cave!!(y)!!(x-1), cave!!(y)!!(x+1)) of
                        (False,_   ) -> sand ((x-1),(y)) cave
                        (True,False) -> sand ((x+1),(y)) cave
                        (True,True ) -> (insert (x) (y-1) cave, (x,y))

normalize :: [[Rock]] -> (Int,[[Rock]])
normalize rs = let xmin = minimum . map (minimum . map fst) $ rs
                   offset' = xmin-offset
               in (500-offset', [[(x-offset', y) | (x,y) <- yrs ] | yrs <- rs])

startcave :: (Int,[[Rock]]) -> (Int,(Int,Int),Int,[[Bool]])
startcave (i,rs) = (i, (0,0), 0, addFloor . addFloor $ addRocks rs [[False | x <- [1..((+offset) .maximum . map (maximum . map fst) $rs)]]| y <- [1..((+2) . maximum . map (maximum . map snd) $ rs)]])

addFloor :: Cave -> Cave
addFloor c = c ++ [take (length (head c)) $ repeat True]

addRocks :: [[Rock]] -> Cave -> Cave
addRocks rs cave = foldl addrock cave rs

addrock :: Cave -> [Rock] -> Cave
addrock cave rocks = foldl (\c -> \(x,y) -> insert x y c) cave (rockPoints rocks)

rockPoints :: [Rock] -> [(Int,Int)]
rockPoints [(x,y)] = [(x,y)]
rockPoints ((x1,y1):(x2,y2):rest) = [(x,y) | x <- [(min x1 x2)..(max x1 x2)], y <- [(min y1 y2)..(max y1 y2)]] ++ rockPoints ((x2,y2):rest)

-- 498,4 -> 498,6 -> 496,6
parseRock :: String -> [Rock]
parseRock input = case parse (try rocks) "parsein" input of
    Left err -> error $ show err
    Right a -> a

rock :: Parsec String () Rock
rock = do
    x <- read <$> many1 digit
    _ <- string ","
    y <- read <$> many1 digit
    return $ (x,y)

rocks :: Parsec String () [Rock]
rocks = rock `sepBy` (string " -> ")

insert :: Int -> Int -> Cave -> Cave
insert x y cave = take y cave ++ [insertx x (cave!!y)] ++ drop (y+1) cave
insertx x cave = take x cave ++ [True] ++ drop (x+1) cave