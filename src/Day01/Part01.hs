module Day01.Part01 (solution) where

solution :: String -> Int
solution = sum . map parseInt . lines
    where
        parseInt:: String -> Int
        parseInt ('+' : xs) = read xs
        parseInt s = read s

