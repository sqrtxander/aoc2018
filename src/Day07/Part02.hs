module Day07.Part02 (solution) where

import Data.Bifunctor (second)
import Data.Char (ord)
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

getTime :: [(Char, [Char])] -> Int
getTime graph = go (getAvail graph) [] graph
  where
    workers = 5
    offset = 60
    go :: [Char] -> [(Char, Int)] -> [(Char, [Char])] -> Int
    go _ _ [] = 0
    go avail working g
        | length working < workers && not (null avail) =
            let chosen = minimum avail
                newAvail = filter (/= chosen) avail
                newWorking = (chosen, ord chosen - 64 + offset) : working
             in go newAvail newWorking g
        | length working == workers || (null avail && not (null working)) =
            let sortedWorkers = sortBy (compare `on` snd) working
                minLeft = snd $ head sortedWorkers
                minChars = map fst $ takeWhile (\x -> snd x == minLeft) sortedWorkers
                newWorking = (filter (\x -> snd x > 0) $ map (\(c, d) -> (c, d - minLeft)) working)
                newG = foldr removeNode g minChars
                newAvail = filter (`notElem` map fst working) $ getAvail newG
             in minLeft + go newAvail newWorking newG
        | otherwise = error "what did we hit?"

solution :: String -> Int
solution s = getTime revGraph
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
