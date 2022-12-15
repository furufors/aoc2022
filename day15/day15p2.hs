#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
import Text.Parsec
type Pos = (Int,Int)
data Data = Data (Pos,Pos) deriving (Show, Eq) -- (Sensor, Beacon)
data Exclusion = Exclusion (Int,Pos,Pos)

main :: IO ()
main = interact $ show . exclusions . toExclusion . map parsein . lines
upLim = 4000000

toExclusion :: [Data] -> [Exclusion]
toExclusion = reverse . sortBy maxManhattan . map (\(Data (a,b)) -> Exclusion (manhattan a b, a, b))
    where
        manhattan :: Pos -> Pos -> Int
        manhattan (a,b) (c,d) = abs (a-c) + abs (b-d)
        maxManhattan (Exclusion (a,_,_)) (Exclusion (b,_,_)) = compare a b

exclusions :: [Exclusion] -> Int
exclusions ds =  head [ x * 4000000 + y | y <- [0..upLim], let rs = merge $ exclusionsY y ds, not (singleLarge rs), x <- [0..upLim], all (not . inside x) (rs)]
    where
        inside x (a,b) = a < x && b > x
        merge :: [(Int, Int)] -> [(Int, Int)]
        merge = converge (foldr go [] . sort . fmap ab)
            where
                go xy [] = [xy]
                go (x,y) ((a,b):rest)
                    | y > b = (x,y):rest
                    | y > a = (x,b):rest
                    | otherwise = (x,y):((a,b):rest)
                ab (a,b) = if a < b then (a,b) else (b,a)

        converge :: Eq a => (a -> a) -> a -> a
        converge f a = let a' = f a in if a == a' then a else converge f a'

        singleLarge :: [(Int, Int)] -> Bool
        singleLarge [(a,b)] | a <= 0 && b >= upLim = True
        singleLarge a = False

        exclusionsY :: Int -> [Exclusion] -> [(Int,Int)]
        exclusionsY y es = [ (a-dx,a+dx) | (Exclusion (d,(a,b),_)) <- es, let dx = max 0 (1+d-(abs (y-b))) ]

parsein :: String -> Data
parsein input = case parse (try sensorData) "parsein" input of
    Left err -> error $ show err
    Right a -> a
    where
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
        parseInt = (string "-" >> parseInt >>= (\s -> return $ (-1) * s) ) <|> (read <$> many1 digit)