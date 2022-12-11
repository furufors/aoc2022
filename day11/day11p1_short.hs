import Data.List
data Monkey = Monkey { ident :: Int,  items :: [] Int,  operation :: Int -> Int,  test :: Int -> Bool,  ifTrue :: Int,  ifFalse :: Int,  inspections :: Int}
main = putStrLn . show . (\(a:b:_) -> a * b) . reverse . sort . map inspections . (!!20) $ iterate (\ms -> foldl turn ms [0..(length ms - 1)]) ma
turn ms i = let changes = [(if (test (ms !! i)) worry then ifTrue (ms !! i) else ifFalse (ms !! i), worry) | item <- items (ms !! i), let worry = ((operation (ms !! i)) item) `div` 3]
            in applyChanges changes ((take i ms) ++ [((\x -> x {items = []}).(\x -> x {inspections = inspections x + length changes})) (ms !! i)] ++ (drop (i+1) ms))
applyChanges [] ms = ms
applyChanges ((i,worry):rest) ms = applyChanges rest (take i ms ++ ((\x -> x { items = items x ++ [worry]}) (ms !! i):drop (i+1) ms))

ma :: [] Monkey
ma = [ Monkey {ident = 0, items = [98,89,52], operation = (\x -> x * 2), test = (\x -> x `mod` 5 == 0), ifTrue = 6, ifFalse = 1, inspections = 0}
     , Monkey {ident = 1, items = [57, 95, 80, 92, 57, 78], operation = (\x -> x * 13), test = (\x -> x `mod` 2 == 0), ifTrue = 2, ifFalse = 6, inspections = 0}
     , Monkey {ident = 2, items = [82, 74, 97, 75, 51, 92, 83], operation = (\x -> x + 5), test = (\x -> x `mod` 19 == 0), ifTrue = 7, ifFalse = 5, inspections = 0}
     , Monkey {ident = 3, items = [97, 88, 51, 68, 76], operation = (\x -> x + 6), test = (\x -> x `mod` 7 == 0), ifTrue = 0, ifFalse = 4, inspections = 0}
     , Monkey {ident = 4, items = [63], operation = (\x -> x + 1), test = (\x -> x `mod` 17 == 0), ifTrue = 0, ifFalse = 1, inspections = 0}
     , Monkey {ident = 5, items = [94, 91, 51, 63], operation = (\x -> x + 4), test = (\x -> x `mod` 13 == 0), ifTrue = 4, ifFalse = 3, inspections = 0}
     , Monkey {ident = 6, items = [61, 54, 94, 71, 74, 68, 98, 83], operation = (\x -> x + 2), test = (\x -> x `mod` 3 == 0), ifTrue = 2, ifFalse = 7, inspections = 0}
     , Monkey {ident = 7, items = [90,56], operation = (\x -> x * x), test = (\x -> x `mod` 11 == 0), ifTrue = 3, ifFalse = 5, inspections = 0}
     ]