#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Foldable (toList)
import Data.Sequence as Seq hiding (length,zip)
import System.TimeIt
type CircularList = Seq (Int, Int)

main :: IO ()
main = timeIt $ interact $ show . run . zip [1..] . map (*811589153) . map read . lines

run :: [(Int, Int)] -> Int
run ps =
    let len = length ps
        ps' = toList . Seq.mapWithIndex (const snd) . align . (!!10) . iterate fun . Seq.fromList $ ps
        fun = \seq -> foldl step seq [1..len]
        align :: CircularList -> CircularList
        align (x:<|xs) = if snd x == 0 then x <| xs else align (xs |> x)
        align xs = xs

        step :: CircularList -> Int -> CircularList
        step cl p =
            let (i,e) = case findIndexL ((==p) . fst) cl of
                    Just i -> (i, index cl i)
                    Nothing -> error $ "Couldn't find elem with index: " ++ show i
                i' = (i + (snd e)) `mod` (len - 1)
                (left, right) = Seq.splitAt i' $ Seq.deleteAt i cl
            in (left |> e )>< right
    in sum . map (\i -> ps'!!(i `mod` len)) $ [1000,2000,3000]