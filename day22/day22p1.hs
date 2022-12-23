#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
import Data.Char
import Data.Maybe (fromJust)
import Debug.Trace
type Map = [[Cell]]
type Key = [Inst]
type Pos = (Dir,(Int,Int))
data Inst = Rotation Rot | Steps Int
data Rot = CW | CCW
data Dir = U | L | D | R deriving (Show,Eq)
data Cell = Empty | Wall | Floor deriving (Show,Eq)

main :: IO ()
main = interact $ show . solve . parse . lines

disp :: Map -> String
disp m = intercalate "\n" $ map (map toChar) m

solve :: (Map, Key) -> Int
solve (m,k) =
    let startx = (+1) . fromJust $ findIndex (==Floor) (m!!1)
        start = (R, (startx,1))
        step :: Pos -> Inst -> Pos
        step (d,p) (Rotation r) = (rotate r d, p)
        step (d,p) (Steps n) = (d, walk n p d)
        walk 0 p d = p
        walk n (x,y) d = walk (n-1) (testStep (x,y) d) d
        testStep (x,y) d =
            let x' = x + xf d
                y' = y + yf d
            in case m!!y'!!x' of
                Empty -> case head $ dropWhile ((== Empty) . fst) [(m!!a!!b, (b,a)) | s <- [1..], let a = (y + (yf d) * s) `mod` length m, let b = (x + (xf d) * s) `mod` length (m!!0)] of
                    (Wall,_)  -> (x,y)
                    (Floor,t) -> t
                Floor -> (x',y')
                Wall -> (x,y)
        (d,(x,y)) = foldl step start k
    in y*1000 + x * 4 + toInt d

toInt :: Dir -> Int
toInt U = 3
toInt L = 2
toInt D = 1
toInt R = 0

toChar :: Cell -> Char
toChar Empty = ' '
toChar Wall  = '#'
toChar Floor = '.'

rotate CW U = R
rotate CW L = U
rotate CW D = L
rotate CW R = D
rotate CCW U = L
rotate CCW L = D
rotate CCW D = R
rotate CCW R = U

xf R = 1
xf L = -1
xf _ = 0
yf U = -1
yf D = 1
yf _ = 0

parse :: [String] -> (Map, Key)
parse ss =  (map (map parseCell) . squareIt . init $ init ss, parseKey $ last ss)

parseKey :: String -> Key
parseKey [] = []
parseKey s  = case takeWhile isDigit s of
    [] -> case head s of
            'L' -> Rotation CCW : parseKey (tail s)
            'R' -> Rotation CW : parseKey (tail s)
    ds -> (Steps $ read ds) : parseKey (dropWhile isDigit s)

parseCell :: Char -> Cell
parseCell ' ' = Empty
parseCell '.' = Floor
parseCell '#' = Wall
parseCell  x  = error $ "Cannot parse char: " ++ [x]

squareIt :: [String] -> [String]
squareIt ss = let maxLen = 2 + maximum (map length ss)
                  header = take maxLen $ repeat ' '
                  pad s = ' ':(s ++ repeat ' ')
                  body = map (take maxLen . pad) ss
              in [header] ++ body ++ [header]