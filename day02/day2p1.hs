#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
data RPS = Rock | Paper | Scissors deriving Eq
type Round = (RPS,RPS)

main :: IO ()
main = interact $ show . sum . map  (score . parse) . lines

parse :: String -> Round
parse s = let x = head $ take 1 s
              y = head . take 1 $ drop 2 s
          in (elf x, player y)

elf, player :: Char -> RPS
elf 'A' = Rock
elf 'B' = Paper
elf 'C' = Scissors
player    'X' = Rock
player    'Y' = Paper
player    'Z' = Scissors

score :: Round -> Int
score r = playScore r + roundScore r

playScore :: Round -> Int
playScore (_,Rock)     = 1
playScore (_,Paper)    = 2
playScore (_,Scissors) = 3

roundScore :: Round -> Int
roundScore (Rock,Paper)     = 6
roundScore (Paper,Scissors) = 6
roundScore (Scissors,Rock)  = 6
roundScore (x,y) | x == y   = 3 -- Draw
roundScore ________________ = 0