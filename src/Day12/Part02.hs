module Day12.Part02 (solution) where

import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

type Rules = Set (Bool, Bool, Bool, Bool, Bool)

parseInput :: String -> (Set Int, Rules)
parseInput s = (pots, rules)
  where
    ls = lines s
    initState = drop (length "initial state: ") (head ls)
    pots = S.fromList [i | (i, '#') <- zip [0 ..] initState]
    to5 [a, b, c, d, e] = Just (a, b, c, d, e)
    to5 _ = Nothing
    rules =
        S.fromList
            [ rule
            | ln <- drop 2 ls
            , let (pat, rest) = splitAt 5 ln
            , rest == " => #"
            , Just rule <- [to5 $ map (== '#') pat]
            ]

normalize :: Set Int -> (Set Int, Int)
normalize pots = (S.map (+ negate minP) pots, minP)
  where
    minP = minimum pots

step :: Rules -> Set Int -> Set Int
step rules pots =
    S.fromList
        [ i
        | i <- [(minP - 2) .. (maxP + 2)]
        , let pattern =
                ( has (i - 2)
                , has (i - 1)
                , has i
                , has (i + 1)
                , has (i + 2)
                )
        , pattern `S.member` rules
        ]
  where
    has x = x `S.member` pots
    minP = minimum pots
    maxP = maximum pots

runGenerations :: Int -> Rules -> Set Int -> Set Int
runGenerations target rules = go M.empty 0
  where
    go seen gen pots
        | gen == target = pots
        | otherwise =
            let (norm, shift) = normalize pots
             in case M.lookup norm seen of
                    Just prevGen ->
                        let cycleLen = gen - prevGen
                            remaining = target - gen
                            skip = remaining `mod` cycleLen
                            finalShift = shift + (remaining - skip)
                            finalPots = S.map (+ finalShift) norm
                         in iterate (step rules) finalPots !! skip
                    Nothing ->
                        let seen' = M.insert norm gen seen
                         in go seen' (gen + 1) $ step rules pots

solution :: String -> Int
solution s = sum $ runGenerations 50000000000 rules pots
  where
    (pots, rules) = parseInput s
