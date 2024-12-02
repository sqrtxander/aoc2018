import qualified Data.IntSet as IntSet
import System.IO

parseInt :: String -> Int
parseInt ('+' : xs) = read xs -- Remove leading '+' if present
parseInt s = read s

findFound :: [Int] -> Int -> IntSet.IntSet -> Int
findFound (x : xs) y set
    | IntSet.member y set = y
    | otherwise = findFound xs (x + y) (IntSet.insert y set)

solve :: [String] -> Int
solve rows = findFound (map parseInt $ cycle rows) 0 IntSet.empty

main :: IO ()
main = do
    rows <- lines <$> readFile "input.in"
    print $ solve rows
