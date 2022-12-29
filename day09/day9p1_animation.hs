#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
{-# LANGUAGE BangPatterns #-}
import Control.DeepSeq
import Data.List
import Terminal.Game
import Control.Parallel
import Control.Parallel.Strategies
import Data.Set (Set)
import qualified Data.Set as S
import Data.HashMap (Map)
import qualified Data.HashMap as M
type GamePlan = Map Pos Int
data Instruction = R | U | D | L deriving (Eq, Show, Ord)
type Pos = (Int,Int)

main :: IO ()
main = do
     inp <- getContents
     let insts = concat . map parse . lines $ inp
     let pos = force $ zip [0..] (follow (0,0) (0,0) [] insts)
     let states = force $ foldl' insertPos M.empty pos
     playGame Game { gTPS = 10
                   , gInitState = 1
                   , gLogicFunction = \g s e -> case e of
                         Tick -> s + 1
                         (KeyPress ' ') -> error "Quit game"
                         (KeyPress _) -> s
                   , gDrawFunction = \g s -> blankPlaneFull g & (0, 0) % disp (states, s, snd (pos!!s))
                   , gQuitFunction = \s -> s == length insts - 3
                   }
     return ()

insertPos :: GamePlan -> (Int,Pos) -> GamePlan
insertPos gp (i,p) = case M.lookup p gp of
     Just j -> if i < j then M.insert p i gp else gp
     Nothing -> M.insert p i gp

disp :: (GamePlan, Int, Pos) -> Plane
disp (gp, i, focus) =
     let (x,y) = focus
         checkPoint :: Pos -> Char
         checkPoint p = case M.lookup p gp of
               Nothing -> ' '
               Just j -> if j <= i then '#' else ' '
         xmin = x-40
         xmax = x+40
         ymin = y-20
         ymax = y+20
         {-!psfil = S.filter (\(x,y) -> xmin <= x && x <= xmax && ymin <= y && y <= ymax) ps -}
         !str = intercalate "\n" . parMap rpar (map checkPoint) $ [ [ (a, b) | a <- [xmin..xmax]] | b <- [ymin..ymax] ]
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