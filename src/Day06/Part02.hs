module Day06.Part02 (solution) where

import Data.Either (partitionEithers)
import Text.Parsec
import Text.Parsec.String (Parser)

type Coord = (Int, Int)

coordParser :: Parser Coord
coordParser = do
    x <- many1 digit
    _ <- string ", "
    y <- many1 digit
    return (read x, read y)

parseRow :: String -> Either ParseError Coord
parseRow = parse coordParser ""

getDistance :: Coord -> Coord -> Int
getDistance p q = abs (fst p - fst q) + abs (snd p - snd q)

solution :: String -> Int
solution s
    | not $ null errors = error "Parsing failed for one or more rows."
    | otherwise =
        let dist = 10000
            pad = dist `div` length points
            minX = minimum $ map fst points
            maxX = maximum $ map fst points
            minY = minimum $ map snd points
            maxY = maximum $ map snd points
            grid =
                [ (x, y)
                | x <- [min minX (maxX - pad) .. max maxX (minX + pad)]
                , y <- [min minY (maxY - pad) .. max maxY (minY + pad)]
                ]
         in length $ filter (\x -> sum (map (getDistance x) points) < dist) grid
  where
    parsed = map parseRow $ lines s
    (errors, points) = partitionEithers parsed
