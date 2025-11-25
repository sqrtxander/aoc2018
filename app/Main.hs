module Main (main) where

import Inputs (getInputPath)
import Modules (getModule)
import Solution (Solution)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Text.Read (readMaybe)

data Day = Day
    { solution :: String -> Solution
    , input :: FilePath
    }

main :: IO ()
main = getArgs >>= parse >>= \d -> readFile (input d) >>= print . solution d

parse :: [String] -> IO Day
parse ["-h"] = usage >> exitSuccess
parse [d, p] = do
    let dInt = readMaybe d :: Maybe Int
    let pInt = readMaybe p :: Maybe Int
    let sol = getModule dInt pInt
    path <- getInputPath dInt
    case (sol, path) of
        (Just f, Just i) -> return Day{solution = f, input = i}
        (Nothing, Just _) -> putStrLn "Solution not yet implemented" >> exitFailure
        (Just _, Nothing) -> putStrLn "Input not found" >> exitFailure
        (Nothing, Nothing) -> usage >> exitFailure
parse _ = usage >> exitFailure

usage :: IO ()
usage =
    putStrLn "Usage: aoc2018 DAY PART"
        >> putStrLn "DAY  in 1 .. 25 (inclusive)"
        >> putStrLn "PART in 1 ..  2 (inclusive)"
