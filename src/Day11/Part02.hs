module Day11.Part02 (solution) where

import qualified Data.Array as A
import qualified Data.Array.Unboxed as U

singleCellPower :: Int -> (Int, Int) -> Int
singleCellPower n (x, y) = hund ((rid * y + n) * rid) - 5
  where
    rid = x + 10
    hund m = (m `div` 100) `mod` 10

getCellsPower :: Int -> U.UArray (Int, Int) Int
getCellsPower n = U.array bounds [(c, singleCellPower n c) | c <- U.range bounds]
  where
    bounds = ((1, 1), (300, 300))

maxSquare :: U.UArray (Int, Int) Int -> (Int, Int, Int)
maxSquare g =
    snd $
        maximum
            [ (at (x, y) s, (x, y, s))
            | s <- [fst minI .. fst maxI]
            , (x, y) <- U.range (minI, (fst maxI - (s - 1), snd maxI - (s - 1)))
            ]
  where
    (minI, maxI) = U.bounds g
    ps = partials g
    at (x, y) s =
        at' (x + s - 1, y + s - 1)
            - at' (x - 1, y + s - 1)
            - at' (x + s - 1, y - 1)
            + at' (x - 1, y - 1)
    at' p
        | A.inRange (U.bounds g) p = ps A.! p
        | otherwise = 0

partials :: U.UArray (Int, Int) Int -> A.Array (Int, Int) Int
partials g = ps
  where
    ps = A.listArray (U.bounds g) . map partial $ U.indices g
    partial (x, y) =
        g U.! (x, y)
            + at (x - 1, y)
            + at (x, y - 1)
            - at (x - 1, y - 1)
    at p
        | A.inRange (U.bounds g) p = ps A.! p
        | otherwise = 0

showAns :: (Show a1, Show a2, Show a3) => (a1, a2, a3) -> [Char]
showAns (x, y, s) = show x ++ "," ++ show y ++ "," ++ show s

solution :: String -> String
solution = showAns . maxSquare . getCellsPower . read
