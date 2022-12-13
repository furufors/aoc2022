#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
data Packet = Val Int | Group [Packet] deriving Show
type PacketPair = (Packet,Packet)

main :: IO ()
main = interact $ show . score 1 . map comparePacketPair . parsein . lines

score :: Int -> [Ordering] -> Int
score i [] = 0
score i (LT:rest) = i + score (i+1) rest
score i (EQ:rest) = score (i+1) rest
score i (GT:rest) = score (i+1) rest

comparePacketPair :: PacketPair -> Ordering
comparePacketPair (Val a, Val b) = compare a b
comparePacketPair (Val a, Group []) = GT
comparePacketPair (Val a, Group bs) = comparePacketPair (Group [Val a], Group bs)
comparePacketPair (Group [], Val b) = LT
comparePacketPair (Group as, Val b) = comparePacketPair (Group as, Group [Val b])
comparePacketPair (Group [], Group []) = EQ
comparePacketPair (Group [], Group bs) = LT
comparePacketPair (Group as, Group []) = GT
comparePacketPair (Group as, Group bs) = case comparePacketPair (head as, head bs) of
        LT -> LT
        GT -> GT
        EQ -> comparePacketPair (Group (tail as),Group (tail bs))

parsein :: [String] -> [PacketPair]
parsein [        ] = []
parsein ("":ss) = parsein ss
parsein (s1:s2:ss) = (parsePacket s1, parsePacket s2):parsein ss

parsePacket :: String -> Packet
parsePacket input = case parse (try group <|> try val) "parsein" input of
    Left err -> error $ show err
    Right a -> a

val :: Parsec String () Packet
val = do
    x <- read <$> many1 digit
    return $ Val x

group :: Parsec String () Packet
group = do
    _ <- string "["
    ns <- (val <|> group) `sepBy` (string ",")
    _ <- string "]"
    return $ Group ns