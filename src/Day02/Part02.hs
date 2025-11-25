module Day02.Part02 (solution) where

removeDiffs' :: Int -> String -> String -> (String, Bool)
removeDiffs' n (x : xs) (y : ys)
    | n > 1 = ("", False)
    | x == y =
        let (rest, ok) = removeDiffs' n xs ys
            in (x : rest, ok)
    | otherwise = removeDiffs' (n + 1) xs ys
removeDiffs' n "" ""
    | n == 1 = ("", True)
    | otherwise = ("", False)
removeDiffs' _ (_ : _) _ = error "Strings are of differing lengths."
removeDiffs' _ _ (_ : _) = error "Strings are of differing lengths."

removeDiffs :: String -> String -> (String, Bool)
removeDiffs = removeDiffs' 0

solution :: String -> String
solution s
    | length filtered == 2 = fst $ head filtered
    | otherwise = error $ "Non-one numbe of solutions (" ++ show (length filtered `div` 2) ++ ")." ++ show pairs
  where
    rows = lines s
    pairs = [removeDiffs x y | x <- rows, y <- rows]
    filtered = filter snd pairs
