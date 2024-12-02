import Data.List (group, sort)
import System.IO

getDiffCount :: String -> String -> Int
getDiffCount [] _ = 0
getDiffCount (x : xs) (y : ys)
    | x /= y = 1 + getDiffCount xs ys
    | otherwise = getDiffCount xs ys

removeFirstDiff :: String -> String -> String
removeFirstDiff (x : xs) (y : ys)
    | x /= y = xs
    | otherwise = x : removeFirstDiff xs ys

solve :: [String] -> String
solve rows =
    let pair = [(x, y) | x <- rows, y <- rows, getDiffCount x y == 1]
    in  uncurry removeFirstDiff (head pair)

main :: IO ()
main = do
    rows <- lines <$> readFile "input.in"
    putStrLn $ solve rows
