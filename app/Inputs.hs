module Inputs (getInputPath) where

import Paths_aoc2018 (getDataFileName)
import System.Directory
import System.FilePath

padZeros :: String -> Int -> String
padZeros str len
    | len <= length str = str
    | otherwise = replicate (len - length str) '0' ++ str

getInputPath :: Maybe Int -> IO (Maybe FilePath)
getInputPath (Just d) = do
    fp <- getDataFileName $ "src" </> ("Day" ++ padZeros (show d) 2) </> "input.in"
    exists <- doesFileExist fp
    if exists
        then return (Just fp)
        else return Nothing
getInputPath Nothing = return Nothing
