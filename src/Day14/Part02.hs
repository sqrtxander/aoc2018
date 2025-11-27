module Day14.Part02 (solution) where

import Data.Char (digitToInt)
import Data.Sequence (Seq)
import qualified Data.Sequence as S

makeRecipe :: Int -> Int
makeRecipe target = go (S.fromList [3, 7]) 0 1
  where
    targetSeq :: Seq Int
    targetSeq = S.fromList . map digitToInt $ show target
    tick :: (Seq Int, Int, Int) -> (Seq Int, Int, Int, Seq Int, Seq Int, Seq Int)
    tick (scores, elf1, elf2) =
        let score1 = S.index scores elf1
            score2 = S.index scores elf2
            (left, toMatch) = S.splitAt (S.length scores - S.length targetSeq - 2) scores
            toMatch1 = S.take (S.length targetSeq) toMatch
            toMatch2 = S.take (S.length targetSeq) (S.drop 1 toMatch)
            s = score1 + score2
            new =
                if s >= 10
                    then S.singleton 1 S.|> (s `mod` 10)
                    else S.singleton (s `mod` 10)
            scores' = scores S.>< new
            len = S.length scores'
            elf1' = (elf1 + 1 + score1) `mod` len
            elf2' = (elf2 + 1 + score2) `mod` len
         in (scores', elf1', elf2', left, toMatch1, toMatch2)
    go :: Seq Int -> Int -> Int -> Int
    go scores elf1 elf2
        | targetSeq == toMatch1 = S.length left
        | targetSeq == toMatch2 = S.length left + 1
        | otherwise = go scores' elf1' elf2'
      where
        (scores', elf1', elf2', left, toMatch1, toMatch2) = tick (scores, elf1, elf2)

solution :: String -> Int
solution = makeRecipe . read
