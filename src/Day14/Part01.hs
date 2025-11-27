module Day14.Part01 (solution) where

import Data.Foldable (Foldable (toList))
import Data.Sequence (Seq)
import qualified Data.Sequence as S

makeRecipe :: Int -> String
makeRecipe target = go (S.fromList [3, 7]) 0 1
  where
    tick :: (Seq Int, Int, Int) -> (Seq Int, Int, Int)
    tick (scores, elf1, elf2) =
        let score1 = S.index scores elf1
            score2 = S.index scores elf2
            s = score1 + score2
            new =
                if s >= 10
                    then S.singleton 1 S.|> (s `mod` 10)
                    else S.singleton (s `mod` 10)
            scores' = scores S.>< new
            len = S.length scores'
            elf1' = (elf1 + 1 + score1) `mod` len
            elf2' = (elf2 + 1 + score2) `mod` len
         in (scores', elf1', elf2')
    go :: Seq Int -> Int -> Int -> String
    go scores elf1 elf2
        | length scores > (target + 10) = concatMap show . toList . S.take 10 $ S.drop target scores
        | otherwise = go scores' elf1' elf2'
      where
        (scores', elf1', elf2') = tick (scores, elf1, elf2)

solution :: String -> String
solution = makeRecipe . read
