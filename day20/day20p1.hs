#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Foldable (toList)
import Data.Sequence as Seq hiding (length,zip)
import System.TimeIt
type CircularList = Seq (Int, Int)

main :: IO ()
main = timeIt $ interact $ show . run . zip [1..] . map read . lines

run :: [(Int, Int)] -> Int
run ps =
    let len = length ps
        ps' = toList . Seq.mapWithIndex (const snd) . align . foldl step (Seq.fromList ps) $ [1..len]

        align :: CircularList -> CircularList
        align (x:<|xs) = if snd x == 0 then x <| xs else align (xs |> x)
        align xs = xs

        step :: CircularList -> Int -> CircularList
        step cl pos =
            let (i,e) = case findIndexL ((==pos) . fst) cl of
                    Just i -> (i, index cl i)
                    Nothing -> error $ "Couldn't find elem with index: " ++ show i
                newIndex = (i + (snd e)) `mod` (len - 1)
                (left, right) = Seq.splitAt newIndex $ Seq.deleteAt i cl
            in (left |> e )>< right
    in sum . map (\i -> ps'!!(i `mod` len)) $ [1000,2000,3000]