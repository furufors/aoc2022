#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
{-# LANGUAGE LambdaCase #-}
import Data.List
data Instruction = R | U | D | L deriving (Eq, Show, Ord)
type Pos = (Int,Int)

main :: IO ()
main = interact $ show . length . nub . sort . last . take 9 . iterate (followPos (0,0) []) . follow (0,0) (0,0) []. concat . map parse . lines

parse :: String -> [Instruction]
parse s = let (a,b) = span (/=' ') s in take (read $ tail b) $ repeat (parseInst $ head a)

parseInst :: Char -> Instruction
parseInst = \case { 'R' -> R; 'U' -> U; 'D' -> D; 'L' -> L; x -> error ("Unkown input " ++ [x]) }

follow :: Pos -> Pos -> [Pos] -> [Instruction] -> [Pos]
follow _       _       ps [    ] = reverse ps
follow h t ps (i:is) = let h' = step h i; t' = follow' h' t in follow h' t' (t':ps) is

follow'  :: Pos -> Pos -> Pos
follow' (hx, hy) (tx, ty) = let dx = hx - tx; dy = hy - ty in move (dx,dy)
    where
        move ( 0, 2) = (tx, ty+1)
        move ( 0,-2) = (tx, ty-1)
        move ( 2, 0) = (tx+1, ty)
        move (-2, 0) = (tx-1, ty)
        move d | d `elem` [( 1, 2),( 2, 1),( 2, 2)] = (tx+1, ty+1)
        move d | d `elem` [(-1, 2),(-2, 1),(-2, 2)] = (tx-1, ty+1)
        move d | d `elem` [(-1,-2),(-2,-1),(-2,-2)] = (tx-1, ty-1)
        move d | d `elem` [( 1,-2),( 2,-1),( 2,-2)] = (tx+1, ty-1)
        move (dx,dy) = (tx,ty)

followPos :: Pos -> [Pos] -> [Pos] -> [Pos]
followPos _       ps [          ] = reverse ps
followPos _       ps [_         ] = reverse ps
followPos t ps (_:h:hs) = let t' = follow' h t in followPos t' (t':ps) (h:hs)

step :: Pos -> Instruction -> Pos
step (x,y) R = (x+1,y)
step (x,y) U = (x,y+1)
step (x,y) D = (x,y-1)
step (x,y) L = (x-1,y)

{-  toString :: [Pos] -> String
    toString pos =
        let minx = minimum $ map fst pos ++ [0]
            maxx = maximum $ map fst pos ++ [0]
            miny = minimum $ map snd pos ++ [0]
            maxy = maximum $ map snd pos ++ [0]
        in intercalate "\n" [[if (x,y) == (0,0) then 's' else if b then '#' else '.' | x <- [minx..maxx], let b = (x,y) `elem` pos]| y <- reverse [miny..maxy]] -}