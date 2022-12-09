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
follow _ _ ps [    ] = reverse ps
follow h t ps (i:is) = let h' = step h i; t' = follow' h' t in follow h' t' (t':ps) is

follow'  :: Pos -> Pos -> Pos
follow' (hx, hy) (tx, ty) = let dx = hx - tx; dy = hy - ty in if dx*dx+dy*dy > 2 then (tx + signum dx, ty + signum dy) else (tx,ty)

followPos :: Pos -> [Pos] -> [Pos] -> [Pos]
followPos _ ps [      ] = reverse ps
followPos _ ps [_     ] = reverse ps
followPos t ps (_:h:hs) = let t' = follow' h t in followPos t' (t':ps) (h:hs)

step :: Pos -> Instruction -> Pos
step (x,y) = \case { R -> (x+1,y); U -> (x,y+1); D -> (x,y-1); L -> (x-1,y) }