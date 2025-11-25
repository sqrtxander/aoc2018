module Day01.Part02 (solution) where

import qualified Data.IntSet as IntSet

firstDup' :: IntSet.IntSet -> [Int] -> Int
firstDup' set (x : xs)
    | IntSet.member x set = x
    | otherwise = firstDup' (IntSet.insert x set) xs
firstDup' _ [] = error "No duplicates found."

firstDup :: [Int] -> Int
firstDup = firstDup' IntSet.empty

solution :: String -> Int
solution = firstDup . scanl (+) 0 . map parseInt . cycle . lines
  where
    parseInt :: String -> Int
    parseInt ('+' : xs) = read xs
    parseInt s = read s
