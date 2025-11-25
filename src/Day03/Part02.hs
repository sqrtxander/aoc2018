module Day03.Part02 (solution) where

import Data.Either (partitionEithers)
import Text.Parsec
import Text.Parsec.String (Parser)

type Coord = (Int, Int)
type Claim = (Int, Coord, Coord)

rowParser :: Parser Claim
rowParser = do
    _ <- char '#'
    i <- read <$> many1 digit
    _ <- string " @ "
    x <- read <$> many1 digit
    _ <- char ','
    y <- read <$> many1 digit
    _ <- string ": "
    w <- read <$> many1 digit
    _ <- char 'x'
    h <- read <$> many1 digit
    return (i, (x, y), (x + w - 1, y + h - 1))

parseRow :: String -> Either ParseError Claim
parseRow = parse rowParser ""

hasOverlap :: Claim -> Claim -> Bool
hasOverlap (_, (x1, y1), (x2, y2)) (_, (x3, y3), (x4, y4)) = x1 <= x4 && y1 <= y4 && x2 >= x3 && y2 >= y3

getNonOverlappingClaim' :: [Claim] -> [Claim] -> Int
getNonOverlappingClaim' [] _ = error "No non-overlapping claim."
getNonOverlappingClaim' (c : cs) cs'
    | not $ any (hasOverlap c) $ filter (/= c) cs' = let (cid, _, _) = c in cid
    | otherwise = getNonOverlappingClaim' cs cs'

getNonOverlappingClaim :: [Claim] -> Int
getNonOverlappingClaim cs = getNonOverlappingClaim' cs cs

solution :: String -> Int
solution s
    | not $ null errors = error "Parsing failed for one or more rows."
    | otherwise = getNonOverlappingClaim claims
  where
    parsed = map parseRow $ lines s
    (errors, claims) = partitionEithers parsed
