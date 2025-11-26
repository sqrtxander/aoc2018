module Modules (getModule) where

import qualified Data.Map as Map
import Solution (Solution, wrapSolution)

import Day00.Part01
import Day00.Part02
import Day01.Part01
import Day01.Part02
import Day02.Part01
import Day02.Part02
import Day03.Part01
import Day03.Part02
import Day04.Part01
import Day04.Part02

moduleList :: Map.Map (Int, Int) (String -> Solution)
moduleList =
    Map.fromList
        [ ((0, 1), wrapSolution . Day00.Part01.solution)
        , ((0, 2), wrapSolution . Day00.Part02.solution)
        , ((1, 1), wrapSolution . Day01.Part01.solution)
        , ((1, 2), wrapSolution . Day01.Part02.solution)
        , ((2, 1), wrapSolution . Day02.Part01.solution)
        , ((2, 2), wrapSolution . Day02.Part02.solution)
        , ((3, 1), wrapSolution . Day03.Part01.solution)
        , ((3, 2), wrapSolution . Day03.Part02.solution)
        , ((4, 1), wrapSolution . Day04.Part01.solution)
        , ((4, 2), wrapSolution . Day04.Part02.solution)
        ]

getModule :: Maybe Int -> Maybe Int -> Maybe (String -> Solution)
getModule (Just d) (Just p) = Map.lookup (d, p) moduleList
getModule _ _ = Nothing
