module Day08.Part01 (solution) where

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

sumMetadata :: Node -> Int
sumMetadata (Node ms cs) = sum ms + sum (map sumMetadata cs)

solution :: String -> Int
solution s = sumMetadata root
  where
    (root, _) = parseNode . map (\x -> read x :: Int) $ words s
