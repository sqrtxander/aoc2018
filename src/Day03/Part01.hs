module Day03.Part01 (solution) where

import Data.Either (partitionEithers)
import qualified Data.Set as Set
import Text.Parsec
import Text.Parsec.String (Parser)

type Coord = (Int, Int)
type Claim = (Coord, Coord)

rowParser :: Parser Claim
rowParser = do
    _ <- char '#'
    _ <- many1 digit
    _ <- string " @ "
    x <- read <$> many1 digit
    _ <- char ','
    y <- read <$> many1 digit
    _ <- string ": "
    w <- read <$> many1 digit
    _ <- char 'x'
    h <- read <$> many1 digit
    return ((x, y), (x + w - 1, y + h - 1))

parseRow :: String -> Either ParseError Claim
parseRow = parse rowParser ""

getIntersection :: [Claim] -> Set.Set Coord
getIntersection [] = Set.empty
getIntersection (c : cs) =
    Set.union
        ( foldr
            ( \c' acc ->
                case getOverlap c c' of
                    Just overlap -> Set.union (getCoords overlap) acc
                    Nothing -> acc
            )
            Set.empty
            cs
        )
        $ getIntersection cs

getCoords :: Claim -> Set.Set Coord
getCoords ((x1, y1), (x2, y2)) = foldr Set.insert Set.empty [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]

hasOverlap :: Claim -> Claim -> Bool
hasOverlap ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) = x1 <= x4 && y1 <= y4 && x2 >= x3 && y2 >= y3

getOverlap :: Claim -> Claim -> Maybe Claim
getOverlap ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4))
    | not $ hasOverlap ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) = Nothing
    | otherwise = Just ((max x1 x3, max y1 y3), (min x2 x4, min y2 y4))

solution :: String -> Int
solution s
    | not $ null errors = error "Parsing failed for one or more rows."
    | otherwise = length $ getIntersection claims
  where
    parsed = map parseRow $ lines s
    (errors, claims) = partitionEithers parsed
