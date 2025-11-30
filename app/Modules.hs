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
import Day05.Part01
import Day05.Part02
import Day06.Part01
import Day06.Part02
import Day07.Part01
import Day07.Part02
import Day08.Part01
import Day08.Part02
import Day09.Part01
import Day09.Part02
import Day10.Part01
import Day10.Part02
import Day11.Part01
import Day11.Part02
import Day12.Part01
import Day12.Part02
import Day13.Part01
import Day13.Part02
import Day14.Part01
import Day14.Part02
import Day15.Part01
import Day15.Part02

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
        , ((5, 1), wrapSolution . Day05.Part01.solution)
        , ((5, 2), wrapSolution . Day05.Part02.solution)
        , ((6, 1), wrapSolution . Day06.Part01.solution)
        , ((6, 2), wrapSolution . Day06.Part02.solution)
        , ((7, 1), wrapSolution . Day07.Part01.solution)
        , ((7, 2), wrapSolution . Day07.Part02.solution)
        , ((8, 1), wrapSolution . Day08.Part01.solution)
        , ((8, 2), wrapSolution . Day08.Part02.solution)
        , ((9, 1), wrapSolution . Day09.Part01.solution)
        , ((9, 2), wrapSolution . Day09.Part02.solution)
        , ((10, 1), wrapSolution . Day10.Part01.solution)
        , ((10, 2), wrapSolution . Day10.Part02.solution)
        , ((11, 1), wrapSolution . Day11.Part01.solution)
        , ((11, 2), wrapSolution . Day11.Part02.solution)
        , ((12, 1), wrapSolution . Day12.Part01.solution)
        , ((12, 2), wrapSolution . Day12.Part02.solution)
        , ((13, 1), wrapSolution . Day13.Part01.solution)
        , ((13, 2), wrapSolution . Day13.Part02.solution)
        , ((14, 1), wrapSolution . Day14.Part01.solution)
        , ((14, 2), wrapSolution . Day14.Part02.solution)
        , ((15, 1), wrapSolution . Day15.Part01.solution)
        , ((15, 2), wrapSolution . Day15.Part02.solution)
        ]

getModule :: Maybe Int -> Maybe Int -> Maybe (String -> Solution)
getModule (Just d) (Just p) = Map.lookup (d, p) moduleList
getModule _ _ = Nothing
