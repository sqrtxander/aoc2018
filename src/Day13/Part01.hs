module Day13.Part01 (solution) where

import qualified Data.Array.Unboxed as U
import Data.List (sortBy)
import Data.Ord (comparing)

data Track
    = Vertical
    | Horizontal
    | Intersection
    | CurvePos
    | CurveNeg
    deriving (Eq, Ord, Show)

data Direction
    = UpDir
    | RightDir
    | DownDir
    | LeftDir
    deriving (Eq, Ord, Bounded, Show, Enum)

data TurnDecision
    = GoLeft
    | GoStraight
    | GoRight
    deriving (Eq, Ord, Bounded, Show, Enum)

type Coord = (Int, Int)
type Cart = (Coord, Direction, TurnDecision)

parseInput :: String -> (U.Array Coord (Maybe Track), [Cart])
parseInput s = (U.array bounds parsedArr, drivers)
  where
    trackType 'v' = Just Vertical
    trackType '^' = Just Vertical
    trackType '|' = Just Vertical
    trackType '<' = Just Horizontal
    trackType '>' = Just Horizontal
    trackType '-' = Just Horizontal
    trackType '+' = Just Intersection
    trackType '/' = Just CurvePos
    trackType '\\' = Just CurveNeg
    trackType _ = Nothing
    parseRow :: (Int, String) -> [(Coord, Maybe Track)]
    parseRow (y, row) =
        [ ((x, y), t)
        | (x, t) <- zip [0 ..] $ map trackType row
        ]
    parsedArr :: [(Coord, Maybe Track)]
    parsedArr = concatMap parseRow $ zip [0 ..] $ lines s
    bounds :: (Coord, Coord)
    bounds =
        let coords = map fst parsedArr
            minX = minimum $ map fst coords
            maxX = maximum $ map fst coords
            minY = minimum $ map snd coords
            maxY = maximum $ map snd coords
         in ((minX, minY), (maxX, maxY))
    getDirection 'v' = Just DownDir
    getDirection '^' = Just UpDir
    getDirection '<' = Just LeftDir
    getDirection '>' = Just RightDir
    getDirection _ = Nothing
    parseDriverRow :: (Int, String) -> [Cart]
    parseDriverRow (y, row) =
        [ ((x, y), d, GoLeft)
        | (x, Just d) <- zipWith (\a b -> (a, getDirection b)) [0 ..] row
        ]
    drivers :: [Cart]
    drivers = concatMap parseDriverRow $ zip [0 ..] $ lines s

succ' :: (Bounded a, Eq a, Enum a) => a -> a
succ' n
    | n == maxBound = minBound
    | otherwise = succ n

pred' :: (Bounded a, Eq a, Enum a) => a -> a
pred' n
    | n == minBound = maxBound
    | otherwise = pred n

moveCart :: U.Array Coord (Maybe Track) -> Cart -> Cart
moveCart tracks (pos, dir, turn) = (pos', dir', turn')
  where
    moveDirection (x, y) UpDir = (x, y - 1)
    moveDirection (x, y) DownDir = (x, y + 1)
    moveDirection (x, y) RightDir = (x + 1, y)
    moveDirection (x, y) LeftDir = (x - 1, y)
    intersectionDecide :: Direction -> TurnDecision -> Direction
    intersectionDecide d GoStraight = d
    intersectionDecide d GoLeft = pred' d
    intersectionDecide d GoRight = succ' d
    posCurveDecide :: Direction -> Direction
    posCurveDecide UpDir = RightDir
    posCurveDecide RightDir = UpDir
    posCurveDecide DownDir = LeftDir
    posCurveDecide LeftDir = DownDir
    negCurveDecide :: Direction -> Direction
    negCurveDecide UpDir = LeftDir
    negCurveDecide LeftDir = UpDir
    negCurveDecide DownDir = RightDir
    negCurveDecide RightDir = DownDir
    pos' = moveDirection pos dir
    track = tracks U.! pos'
    (turn', dir') = case track of
        Nothing -> error "Off the rails."
        Just Vertical -> (turn, dir)
        Just Horizontal -> (turn, dir)
        Just Intersection -> (succ' turn, intersectionDecide dir turn)
        Just CurvePos -> (turn, posCurveDecide dir)
        Just CurveNeg -> (turn, negCurveDecide dir)

hasCrashed :: Cart -> [Cart] -> Maybe Coord
hasCrashed (pos, _, _) cs
    | pos `elem` map (\(p, _, _) -> p) cs = Just pos
    | otherwise = Nothing

stepThroughCarts :: U.Array Coord (Maybe Track) -> [Cart] -> [Cart] -> Coord
stepThroughCarts tracks [] carts = stepThroughCarts tracks sortedCarts sortedCarts
  where
    sortedCarts = sortBy (comparing (\((x, y), _, _) -> (y, x))) carts
stepThroughCarts tracks (c : cs) carts =
    let c' = moveCart tracks c
     in case hasCrashed c' carts of
            Just p -> p
            Nothing ->
                let carts' = (c' : filter (/= c) carts)
                 in stepThroughCarts tracks cs carts'

showAns :: Coord -> String
showAns (x, y) = show x ++ "," ++ show y

solution :: String -> String
solution s = showAns $ stepThroughCarts tracks carts carts
  where
    (tracks, carts) = parseInput s
