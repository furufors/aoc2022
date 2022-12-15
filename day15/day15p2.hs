#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
import Text.Parsec
type Pos = (Int,Int)
data Data = Data (Pos,Pos) deriving (Show, Eq) -- (Sensor, Beacon)
data Exclusion = Exclusion (Int,Pos,Pos)

main :: IO ()
main = interact $ show . exclusions . toExclusion . map parsein . lines
offset = 1000000
upLim = 4000000

toExclusion :: [Data] -> [Exclusion]
toExclusion = reverse . sortBy maxManhattan . map (\(Data (a,b)) -> Exclusion (manhattan a b, a, b))

maxManhattan (Exclusion (a,_,_)) (Exclusion (b,_,_)) = compare a b

exclusionsY :: Int -> [Exclusion] -> [(Int,Int)]
exclusionsY y es = [ (a-dx,a+dx) | (Exclusion (d,(a,b),_)) <- es, let dx = max 0 (1+d-(abs (y-b))) ]
exclusionsX :: Int -> [Exclusion] -> [(Int,Int)]
exclusionsX x es = [ (b-dy,b+dy) | (Exclusion (d,(a,b),_)) <- es, let dy = max 0 (1+d-(abs (x-a))) ]


exclusions :: [Exclusion] -> Int
exclusions ds =
    let exy = \y -> exclusionsY y ds
        exx = \x -> exclusionsX x ds
    in head [ x * 4000000 + y | x <- [0..upLim], y <- [0..upLim], all (not . inside x) (exy y) && all (not . inside y) (exx x) ]

manhattan :: Pos -> Pos -> Int
manhattan (a,b) (c,d) = abs (a-c) + abs (b-d)

inside x (a,b) = a < x && b > x

parsein :: String -> Data
parsein input = case parse (try sensorData) "parsein" input of
    Left err -> error $ show err
    Right a -> a

sensorData :: Parsec String () Data
sensorData = do
    _ <- string "Sensor at x="
    a <- parseInt
    _ <- string ", y="
    b <- parseInt
    _ <- string ": closest beacon is at x="
    c <- parseInt
    _ <- string ", y="
    d <- parseInt
    return $ Data ((a,b),(c,d))

parseInt = parseIntN <|> parseIntP
parseIntN = string "-" >> parseInt >>= (\s -> return $ (-1) * s)
parseIntP =  read <$> many1 digit