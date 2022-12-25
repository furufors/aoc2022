#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
{-# LANGUAGE BangPatterns #-}
import Data.List
import Terminal.Game
import Data.Set (Set)
import qualified Data.Set as S
data Instruction = R | U | D | L deriving (Eq, Show, Ord)
type Pos = (Int,Int)

main :: IO ()
main = do
     inp <- getContents
     let insts = concat . map parse . lines $ inp
     let pos = follow (0,0) (0,0) [] insts
     let !states = scanl (flip S.insert) S.empty pos
     playGame Game { gTPS = 10
                   , gInitState = 1
                   , gLogicFunction = \g s e -> s + 1
                   , gDrawFunction = \g s -> blankPlaneFull g & (0, 0) % disp (states!!(s+3),pos!!s)
                   , gQuitFunction = \s -> s == length insts - 3
                   }
     return ()

disp :: (Set Pos, Pos) -> Plane
disp (ps,focus) =
     let (x,y) = focus
         xmin = x-40
         xmax = x+40
         ymin = y-20
         ymax = y+20
         psfil = S.filter (\(x,y) -> xmin <= x && x <= xmax && ymin <= y && y <= ymax) ps
         str = intercalate "\n" $ [ [ if S.member (a,b) psfil then '#' else ' ' | a <- [xmin..xmax]] | b <- [ymin..ymax] ]
     in textBox (xmax-xmin+1) (ymax-ymin+1) str

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