#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
data RPS = Rock | Paper | Scissors deriving Eq
data Outcome = Win | Draw | Lose
type Round = (RPS,Outcome)

main :: IO ()
main = interact $ show . sum . map  (score . parse) . lines

parse :: String -> Round
parse s = let x = head $ take 1 s
              y = head . take 1 $ drop 2 s
          in (elf x, outcome y)

elf :: Char -> RPS
elf 'A' = Rock
elf 'B' = Paper
elf 'C' = Scissors
outcome :: Char -> Outcome
outcome    'X' = Lose
outcome    'Y' = Draw
outcome    'Z' = Win

score :: Round -> Int
score r = playScore r + roundScore r

playScore :: Round -> Int
playScore (Rock, Win) = signScore Paper
playScore (Paper, Win) = signScore Scissors
playScore (Scissors, Win) = signScore Rock
playScore (Rock, Lose) = signScore Scissors
playScore (Paper, Lose) = signScore Rock
playScore (Scissors, Lose) = signScore Paper
playScore (x, Draw) = signScore x

signScore :: RPS -> Int
signScore Rock = 1
signScore Paper = 2
signScore Scissors = 3

roundScore :: Round -> Int
roundScore (_,Lose) = 0
roundScore (_,Draw) = 3
roundScore (_,Win)  = 6