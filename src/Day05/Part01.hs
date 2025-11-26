module Day05.Part01 (solution) where

import Data.Char (isSpace, ord)
import Data.List (dropWhileEnd)

reactOnce :: String -> String
reactOnce [] = []
reactOnce [x] = [x]
reactOnce (x : y : xs)
    | doReact x y = reactOnce xs
    | otherwise = x : reactOnce (y : xs)
  where
    doReact a b = 32 == abs (ord a - ord b)

reactFully' :: String -> String -> String
reactFully' prev curr
    | prev == curr = curr
    | otherwise = reactFully' curr $ reactOnce curr

reactFully :: String -> String
reactFully = reactFully' ""

solution :: String -> Int
solution s = length . reactFully $ dropWhileEnd isSpace s
