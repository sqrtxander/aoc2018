module Day07.Part01 (solution) where

import Data.Bifunctor (second)
import Data.Function (on)
import Data.List (group, groupBy, sort, sortBy)
import Text.Parsec
import Text.Parsec.String (Parser)

rowParser :: Parser (Char, Char)
rowParser = do
    _ <- string "Step "
    a <- upper
    _ <- string " must be finished before step "
    b <- upper
    _ <- string " can begin."
    return (a, b)

parseRow :: String -> (Char, Char)
parseRow s =
    case parse rowParser "" s of
        Right cs -> cs
        Left _ -> error "Parsing failed for one or more rows."

getAvail :: (Foldable t) => [(a1, t a2)] -> [a1]
getAvail [] = []
getAvail (x : xs)
    | null $ snd x = fst x : getAvail xs
    | otherwise = getAvail xs

removeNode :: (Eq a) => a -> [(a, [a])] -> [(a, [a])]
removeNode _ [] = []
removeNode chosen g = map (Data.Bifunctor.second $ filter (/= chosen)) $ filter (\x -> fst x /= chosen) g

getOrder :: [(Char, [Char])] -> String
getOrder graph = go (getAvail graph) graph
  where
    go :: [Char] -> [(Char, [Char])] -> String
    go _ [] = []
    go avail g =
        let chosen = minimum avail
            newGraph = removeNode chosen g
         in chosen : go (getAvail newGraph) newGraph

solution :: String -> String
solution s = getOrder revGraph
  where
    dedup = map head . group . sort
    nodes = dedup . concatMap ((\(x, y) -> [x, y]) . parseRow) $ lines s
    rightNodes = dedup . map (snd . parseRow) $ lines s
    nonRightNodes = filter (`notElem` rightNodes) nodes

    revGraph =
        map (\x -> (x, [])) nonRightNodes
            ++ ( map (\xs -> (snd $ head xs, map fst xs))
                    . groupBy ((==) `on` snd)
                    . sortBy (compare `on` snd)
                    . map parseRow
                    $ lines s
               )
