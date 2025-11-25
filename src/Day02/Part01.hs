module Day02.Part01 (solution) where

import Data.List (group, sort)

solution :: String -> Int
solution s = numWith 2 * numWith 3
  where
    rows = lines s
    contains x t = x `elem` counts t
    counts = map length . group . sort
    numWith x = length $ filter (contains x) rows
