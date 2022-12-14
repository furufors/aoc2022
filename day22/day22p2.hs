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
        gs = (length $ filter (/= Empty) (m!!0)) `div` 2
        step :: Pos -> Inst -> Pos
        step (d,p) (Rotation r) = (rotate r d, p)
        step (d,p) (Steps n) = let (p',d') = walk n p d in (d', p')
        walk 0 p d = (p,d)
        walk n (x,y) d = let (p',d') = testStep (x,y) d in walk (n-1) p' d'
        testStep (x,y) d =
            let x' = x + xf d
                y' = y + yf d
            in if x' < 0 || x' >= 3*gs || y' < 0 || y' >= 4*gs
               then changeFace (x,y) d
               else case m!!y'!!x' of
                        Empty -> changeFace (x,y) d
                        Floor -> ((x',y'), d)
                        Wall ->  ((x,y), d)
        isWall (x,y) = case m!!y!!x of
            Empty -> error "Attempting to move into the void!"
            Wall  -> True
            Floor -> False
        face (x,y) = case (x `div` gs, y `div` gs) of
            (1,0) -> 1
            (2,0) -> 2
            (1,1) -> 3
            (1,2) -> 4
            (0,2) -> 5
            (0,3) -> 6
        changeFace (x,y) d =
            let xm = x `mod` gs
                ym = y `mod` gs
                (p',d') = case (face (x,y), d) of
                    (1,U) -> ((0,         3*gs+xm),R)  --    11112222
                    (1,L) -> ((0,       3*gs-1-ym),R)  --    11112222
                    (2,U) -> ((xm,         4*gs-1),U)  --    11112222
                    (2,R) -> ((2*gs-1,  3*gs-1-ym),L)  --    11112222
                    (2,D) -> ((2*gs-1,      gs+xm),L)  --    3333
                    (3,L) -> ((ym,           2*gs),D)  --    3333
                    (3,R) -> ((2*gs+ym,      gs-1),U)  --    3333
                    (4,D) -> ((gs-1,      3*gs+xm),L)  --    3333
                    (4,R) -> ((3*gs-1,    gs-1-ym),L)  --55554444
                    (5,U) -> ((gs,          gs+xm),R)  --55554444 153203
                    (5,L) -> ((gs,        gs-1-ym),R)  --55554444
                    (6,L) -> ((gs+ym,           0),D)  --55554444
                    (6,D) -> ((2*gs+xm,         0),D)  --6666
                    (6,R) -> ((gs+ym,      3*gs-1),U)  --6666
                                                       --6666
                                                       --6666
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
