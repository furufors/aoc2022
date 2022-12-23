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
    let startx = fromJust $ findIndex (==Floor) (m!!0)
        start = (R, (startx,0))
        gs = length $ filter (/= Empty) (m!!0)
        step :: Pos -> Inst -> Pos
        step (d,p) (Rotation r) = (rotate r d, p)
        step (d,p) (Steps n) = let (p',d') = walk n p d in (d', p')
        walk 0 p d = (p,d)
        walk n (x,y) d = let (p',d') = testStep (x,y) d in walk (n-1) p' d'
        testStep (x,y) d =
            let x' = x + xf d
                y' = y + yf d
            in if x' < 0 || x' >= 4*gs || y' < 0 || y' >= 3*gs
               then changeFace (x,y) d
               else case m!!y'!!x' of
                        Empty -> changeFace (x,y) d
                        Floor -> ((x',y'), d)
                        Wall ->  ((x,y), d)
        isWall (x,y) = case m!!y!!x of
            Empty -> error "Attempting to move into the void!"
            Wall  -> True
            Floor -> False
        face (x,y) = trace (show (x,y)) $ case (x `div` gs, y `div` gs) of
            (2,0) -> 1
            (0,1) -> 2
            (1,1) -> 3
            (2,1) -> 4
            (2,2) -> 5
            (3,2) -> 6
        changeFace (x,y) d =
            let xm = x `mod` gs
                ym = y `mod` gs
                (p',d') = case (face (x,y), d) of
                    (1,U) -> ((gs-1-xm,        gs),D)  --         1111
                    (1,L) -> ((gs + ym,        gs),D)  --         1111
                    (1,R) -> ((4*gs-1,  3*gs-1-ym),L)  --         1111
                    (2,U) -> ((3*gs-1-xm,       0),D)  --         1111
                    (2,L) -> ((4*gs-1-ym,  3*gs-1),U)  -- 222233334444
                    (2,D) -> ((3*gs-1-xm,  3*gs-1),U)  -- 222233334444
                    (3,U) -> ((2*gs,           xm),R)  -- 222233334444
                    (3,D) -> ((2*gs,    3*gs-1-xm),R)  -- 222233334444
                    (4,R) -> ((4*gs-1-ym,    2*gs),D)  --         55556666
                    (5,L) -> ((2*gs-1-ym,  2*gs-1),U)  --         55556666
                    (5,D) -> ((gs-1-xm,    2*gs-1),U)  --         55556666
                    (6,U) -> ((3*gs-1,  2*gs-1-xm),L)  --         55556666
                    (6,D) -> ((0,       2*gs-1-xm),R)
                    (6,R) -> ((3*gs-1,    gs-1-ym),L)
            in if isWall p' then ((x,y),d) else (p', d')
        (d,(x,y)) = foldl step start k
    in (y+1)*1000 + (x+1) * 4 + toInt d

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
squareIt ss = let maxLen = maximum (map length ss)
                  pad s = s ++ repeat ' '
              in map (take maxLen . pad) ss
