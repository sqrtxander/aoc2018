import Data.Either (partitionEithers)
import qualified Data.Set as Set
import System.IO
import Text.Parsec
import Text.Parsec.String (Parser)

type Coord = (Integer, Integer)

type Claim = (Integer, Coord, Coord)

rowParser :: Parser Claim
rowParser = do
    char '#'
    id <- read <$> many1 digit
    string " @ "
    x <- read <$> many1 digit
    char ','
    y <- read <$> many1 digit
    string ": "
    w <- read <$> many1 digit
    char 'x'
    h <- read <$> many1 digit
    return (id, (x, y), (x + w - 1, y + h - 1))

parseRow :: String -> Either ParseError Claim
parseRow = parse rowParser ""

getCoords :: Claim -> Set.Set Coord
getCoords (_, (x1, y1), (x2, y2)) = foldr Set.insert Set.empty [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]

hasOverlap :: Claim -> Claim -> Bool
hasOverlap (_, (x1, y1), (x2, y2)) (_, (x3, y3), (x4, y4)) = x1 <= x4 && y1 <= y4 && x2 >= x3 && y2 >= y3

getOverlap :: Claim -> Claim -> Maybe Claim
getOverlap (xid, (x1, y1), (x2, y2)) (yid, (x3, y3), (x4, y4))
    | not $ hasOverlap (xid, (x1, y1), (x2, y2)) (yid, (x3, y3), (x4, y4)) = Nothing
    | otherwise = Just (xid, (max x1 x3, max y1 y3), (min x2 x4, min y2 y4))

getNonOverlappingClaim :: [Claim] -> [Claim] -> Integer
getNonOverlappingClaim [] _ = error "No non overlapping claim"
getNonOverlappingClaim (c : cs) cs'
    | not $ any (hasOverlap c) $ filter (/= c) cs' = let (id, _, _) = c in id
    | otherwise = getNonOverlappingClaim cs cs'

solve :: [String] -> Integer
solve rows =
    let parsed = map parseRow rows
        (errors, claims) = partitionEithers parsed
    in  if not $ null errors
            then error "Parsing failed for one or more rows"
            else getNonOverlappingClaim claims claims

main :: IO ()
main = do
    rows <- lines <$> readFile "input.in"
    print $ solve rows
