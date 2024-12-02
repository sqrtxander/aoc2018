import Data.List (group, sort)
import System.IO

counts :: String -> [Int]
counts s = map length $ group $ sort s

contains :: Int -> String -> Bool
contains x s = x `elem` counts s

solve :: [String] -> Int
solve rows = length (filter (contains 2) rows) * length (filter (contains 3) rows)

main :: IO ()
main = do
    rows <- lines <$> readFile "input.in"
    print $ solve rows
