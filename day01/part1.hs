import System.IO

parseInteger :: String -> Integer
parseInteger ('+' : xs) = read xs
parseInteger s = read s

solve :: [String] -> Integer
solve rows = sum $ map parseInteger rows

main :: IO ()
main = do
    rows <- lines <$> readFile "input.in"
    print $ solve rows
