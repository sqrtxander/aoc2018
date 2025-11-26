module Day06.Part01 (solution) where

import Data.Either (partitionEithers)
import Data.Foldable (maximumBy, minimumBy)
import Data.Function (on)
import Data.List (group, sort)
import Data.Maybe (mapMaybe)
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

getClosestPoint :: [Coord] -> Coord -> Maybe Coord
getClosestPoint xs x
    | length allClosest == 1 = Just $ snd . head $ sorted
    | otherwise = Nothing
  where
    manhattan p q = abs (fst p - fst q) + abs (snd p - snd q)
    distances = map (\y -> (manhattan x y, y)) xs
    sorted = minimumBy (compare `on` length) . group . sort $ distances
    minDist = fst $ head sorted
    allClosest = takeWhile (\y -> fst y == minDist) sorted

solution :: String -> Int
solution s
    | not $ null errors = error "Parsing failed for one or more rows."
    | otherwise =
        let minX = minimum $ map fst points
            maxX = maximum $ map fst points
            minY = minimum $ map snd points
            maxY = maximum $ map snd points
            grid = [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY]]
            border =
                [(x, y) | x <- [minX .. maxX], y <- [minY - 1, maxY + 1]]
                    ++ [(x, y) | x <- [minX - 1, maxX + 1], y <- [minY .. maxY]]
            infinite = map head . group . sort $ mapMaybe (getClosestPoint points) border
            finite = filter (`notElem` infinite) $ mapMaybe (getClosestPoint points) grid
         in length . maximumBy (compare `on` length) . group . sort $ finite
  where
    parsed = map parseRow $ lines s
    (errors, points) = partitionEithers parsed
