#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
type Rock = (Int,Int,Int)
main :: IO ()
main = interact $ show . countSides . map parseRock . lines

countSides rocks = (6 * length rocks) - sum (adjacent rocks)
adjacent [] = []
adjacent ((x,y,z):rocks) = [if adjPair (abs (a-x)) (abs (b-y)) (abs (c-z)) == 1 then 2 else 0
                 | (a,b,c) <- rocks ] ++ (adjacent rocks)

adjPair 1 0 0 = 1
adjPair 0 1 0 = 1
adjPair 0 0 1 = 1
adjPair _ _ _ = 0

parseRock :: String -> Rock
parseRock input = case parse (try rock) "parsein" input of
    Left err -> error $ show err
    Right a -> a

rock :: Parsec String () Rock
rock = do
    x  <- read <$> many1 digit
    _  <- string ","
    y  <- read <$> many1 digit
    _  <- string ","
    z  <- read <$> many1 digit
    return $ (x,y,z)