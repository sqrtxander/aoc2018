module Day04.Part02 (solution) where

import Control.Arrow ()
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (group, maximumBy, sort)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.Time (LocalTime (localTimeOfDay), TimeOfDay (todMin), utc, utcToLocalTime)
import Data.Time.Clock
import Data.Time.Format
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Read (readMaybe)

data Record = Record
    { time :: UTCTime
    , guard :: Maybe Int
    , awake :: Bool
    }
    deriving (Show, Eq)

instance Ord Record where
    compare r1 r2 = compare (time r1) (time r2)

rowParser :: Parser Record
rowParser = do
    _ <- char '['
    tstr <- manyTill anyChar (char ']')
    _ <- char ' '
    (g, a) <-
        choice
            [ try (string "falls asleep" >> return (Nothing, False))
            , try (string "wakes up" >> return (Nothing, True))
            , do
                _ <- string "Guard #"
                gid <- many1 digit
                _ <- string " begins shift"
                return (readMaybe gid, True)
            ]
    return
        Record
            { time = parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M" tstr
            , guard = g
            , awake = a
            }

parseRow :: String -> Either ParseError Record
parseRow = parse rowParser ""

addGuards' :: Maybe Int -> [Record] -> [Record]
addGuards' _ [] = []
addGuards' (Just gid) (r : rs)
    | isNothing $ guard r = Record{time = time r, guard = Just gid, awake = awake r} : addGuards' (Just gid) rs
    | otherwise = addGuards' (guard r) rs
addGuards' Nothing (r : rs)
    | isNothing $ guard r = error "No Guard found."
    | otherwise = addGuards' (guard r) rs

addGuards :: [Record] -> [Record]
addGuards = addGuards' Nothing

groupOn :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupOn f = unpack . foldl fld Map.empty . reverse
  where
    unpack = fmap snd . Map.toList
    fld m a = case Map.lookup (f a) m of
        Nothing -> Map.insert (f a) [a] m
        Just as -> Map.insert (f a) (a : as) m

asleepMinutes :: [Record] -> [Int]
asleepMinutes = go [] Nothing
  where
    go :: [Int] -> Maybe UTCTime -> [Record] -> [Int]
    go acc _ [] = acc
    go acc Nothing (r : rs)
        | awake r = go acc Nothing rs
        | otherwise = go acc (Just $ time r) rs
    go acc (Just sleepTime) (r : rs)
        | awake r =
            let startMin = todMin . localTimeOfDay $ utcToLocalTime utc sleepTime
                endMin = todMin . localTimeOfDay $ utcToLocalTime utc $ time r
                newMins = [startMin .. endMin - 1]
             in go (acc ++ newMins) Nothing rs
        | otherwise = go acc (Just $ time r) rs

maximumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
maximumOn f = maximumBy (compare `on` f)

toMinutesAsleep :: [Record] -> [(Int, Int, Int)]
toMinutesAsleep rs = map (\xs -> (gid, head xs, length xs)) . group . sort $ asleepMinutes rs
  where
    gid = case guard $ head rs of
        Just g -> g
        Nothing -> error "No GuardID Found"

solution :: String -> Int
solution s
    | not $ null errors = error "Parsing failed for one or more rows."
    | otherwise =
        let grs = maximumOn trd3 . concatMap toMinutesAsleep . groupOn guard $ clean records
            (gid, minute, _) = grs
         in gid * minute
  where
    parsed = map parseRow $ lines s
    (errors, records) = partitionEithers parsed
    clean = addGuards . sort
    trd3 (_, _, z) = z
