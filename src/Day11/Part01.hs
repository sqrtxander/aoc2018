module Day11.Part01 (solution) where

import qualified Data.Array.Unboxed as U
import Data.Foldable (maximumBy)
import Data.Function (on)

singleCellPower :: Int -> (Int, Int) -> Int
singleCellPower n (x, y) = hund ((rid * y + n) * rid) - 5
  where
    rid = x + 10
    hund m = (m `div` 100) `mod` 10

getCellsPower :: Int -> U.UArray (Int, Int) Int
getCellsPower n = U.array bounds [(c, singleCellPower n c) | c <- U.range bounds]
  where
    bounds = ((1, 1), (300, 300))

max3Square :: U.UArray (Int, Int) Int -> (Int, Int)
max3Square g =
    snd $
        maximumBy
            (compare `on` fst)
            [ (go p, p)
            | p <- U.range ((minX, minY), (maxX, maxY))
            ]
  where
    (minI, maxI) = U.bounds g
    minX = fst minI
    maxX = fst maxI - 2
    minY = snd minI
    maxY = snd maxI - 2
    go (x, y) = sum . map (g U.!) $ U.range ((x, y), (x + 2, y + 2))


showAns :: (Show a1, Show a2) => (a1, a2) -> [Char]
showAns (x, y) = show x ++ "," ++ show y

solution :: String -> String
solution = showAns . max3Square . getCellsPower . read
-- solution = show . getCellsPower . read
