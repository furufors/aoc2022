#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
data Instruction = R | U | D | L deriving (Eq, Show, Ord)
type Pos = (Int,Int)

main :: IO ()
main = interact $ show . length . nub . sort . follow (0,0) (0,0) [] . concat . map parse . lines

parse :: String -> [Instruction]
parse s = let (a,b) = span (/=' ') s in take (read $ tail b) $ repeat (parseInst $ head a)

parseInst :: Char -> Instruction
parseInst 'R' = R
parseInst 'U' = U
parseInst 'D' = D
parseInst 'L' = L
parseInst x   = error $ "Unkown input " ++ [x]

follow :: Pos -> Pos -> [Pos] -> [Instruction] -> [Pos]
follow _       _       ps [    ] = ps
follow (hx,hy) (tx,ty) ps (i:is) =
    let (hx',hy') = step (hx,hy) i
        tailMoves = (hx'-tx)*(hx'-tx) + (hy'-ty)*(hy'-ty) > 2
        (tx',ty') = if tailMoves
                    then case i of
                         R -> if ty == hy'
                              then step (tx,ty) R
                              else step (if ty < hy' then step (tx,ty) U else step (tx,ty) D) R
                         U -> if tx == hx'
                              then step (tx,ty) U
                              else step (if tx < hx' then step (tx,ty) R else step (tx,ty) L) U
                         D -> if tx == hx'
                              then step (tx,ty) D
                              else step (if tx < hx' then step (tx,ty) R else step (tx,ty) L) D
                         L -> if ty == hy'
                              then step (tx,ty) L
                              else step (if ty < hy' then step (tx,ty) U else step (tx,ty) D) L
                    else (tx,ty)
    in follow (hx',hy') (tx',ty') ((tx',ty'):ps) is

step :: Pos -> Instruction -> Pos
step (x,y) R = (x+1,y)
step (x,y) U = (x,y+1)
step (x,y) D = (x,y-1)
step (x,y) L = (x-1,y)