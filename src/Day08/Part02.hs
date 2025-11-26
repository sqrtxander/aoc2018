module Day08.Part02 (solution) where

data Node = Node
    { metadata :: [Int]
    , children :: [Node]
    }
    deriving (Show)

parseNode :: [Int] -> (Node, [Int])
parseNode (x : y : xs) = (Node{metadata = _metadata, children = _children}, metadataRest)
  where
    (_children, childrenRest) = parseChildren x xs
    (_metadata, metadataRest) = splitAt y childrenRest
parseNode _ = error "Invalid input."

parseChildren :: Int -> [Int] -> ([Node], [Int])
parseChildren 0 xs = ([], xs)
parseChildren n xs = (child : others, rest2)
  where
    (child, rest1) = parseNode xs
    (others, rest2) = parseChildren (n - 1) rest1

getValue :: Node -> Int
getValue (Node ms []) = sum ms
getValue (Node ms cs) =
    sum
        ( map
            ((getValue . (cs !!)) . (\x -> x - 1))
            (filter (\m -> m > 0 && m <= length cs) ms)
        )

solution :: String -> Int
solution s = getValue root
  where
    (root, _) = parseNode . map (\x -> read x :: Int) $ words s
