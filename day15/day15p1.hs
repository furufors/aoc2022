#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
type Pos = (Int,Int)
data Data = Data (Pos,Pos) deriving (Show, Eq) -- (Sensor, Beacon)

main :: IO ()
main = interact $ show . length . filter id . exclusionsY 2000000 . map parsein . lines
offset = 1000000

exclusionsY :: Int -> [Data] -> [Bool]
exclusionsY y ds =
    let maxx = offset + (maximum $ map (\(Data ((a,b),(c,d))) -> max a c) ds)
        minx = -offset + (minimum $ map (\(Data ((a,b),(c,d))) -> min a c) ds)
    in [ any id [manhattan (x,y) (a,b) <= manhattan (a,b) (c,d) && (x,y) /= (c,d)| (Data ((a,b),(c,d))) <- ds] | x <- [minx..maxx]]

manhattan :: Pos -> Pos -> Int
manhattan (a,b) (c,d) = abs (a-c) + abs (b-d)

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