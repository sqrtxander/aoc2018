module Day09.Part01 (solution) where

import qualified Data.IntMap as Map
import Text.Parsec
import Text.Parsec.String (Parser)

data Circle a = C [a] [a]
    deriving (Show)

rowParser :: Parser (Int, Int)
rowParser = do
    players <- many1 digit
    _ <- string " players; last marble is worth "
    highest <- many1 digit
    _ <- string " points"
    return (read players, read highest)

parseRow :: String -> (Int, Int)
parseRow s = case parse rowParser "" s of
    Right cs -> cs
    Left _ -> error "Parsing failed for one or more rows."

normalize :: Circle a -> Circle a
normalize (C ls []) = C [] (reverse ls)
normalize c = c

moveCW :: Circle a -> Circle a
moveCW (C ls (r : rs)) = C (r : ls) rs
moveCW (C ls []) =
    case reverse ls of
        [] -> C [] []
        (l : rs) -> C [l] rs

moveCCW :: Circle a -> Circle a
moveCCW (C (l : ls) rs) = C ls (l : rs)
moveCCW (C [] rs) =
    case reverse rs of
        [] -> C [] []
        (r : ls) -> C ls [r]

insert :: a -> Circle a -> Circle a
insert x c = C ls (x : rs)
  where
    C ls rs = moveCW $ moveCW c

remove7 :: Circle a -> (a, Circle a)
remove7 c =
    case normalize c7 of
        C _ [] -> error "Nothing to remove"
        C l (x : r) -> (x, normalize $ C l r)
  where
    c7 = iterate moveCCW c !! 7

getScores :: Map.Key -> Int -> Map.IntMap Map.Key
getScores players highest =
    let step (c, scrs) m
            | m `mod` 23 /= 0 =
                (insert m c, scrs)
            | otherwise =
                let (taken, c') = remove7 c
                    p = m `mod` players
                 in (c', Map.insertWith (+) p (m + taken) scrs)
        (_, scores) = foldl step (C [] [0], Map.empty) [1 .. highest]
     in scores

solution :: String -> Int
solution s = maximum $ getScores players highest
  where
    (players, highest) = parseRow s
