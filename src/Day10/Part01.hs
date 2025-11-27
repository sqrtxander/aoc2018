module Day10.Part01 (solution) where

import qualified Data.Bifunctor
import Data.Functor.Identity
import qualified Data.Set as Set
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

type Position = (Int, Int)
type Velocity = (Int, Int)

lexer :: Tok.GenTokenParser String u Data.Functor.Identity.Identity
lexer = Tok.makeTokenParser emptyDef

intParser :: ParsecT String u Identity Int
intParser = fromInteger <$> Tok.integer lexer

rowParser :: Parser (Position, Velocity)
rowParser = do
    _ <- string "position=<"
    _ <- optionMaybe $ char ' '
    posX <- intParser
    _ <- string ", "
    _ <- optionMaybe $ char ' '
    posY <- intParser
    _ <- string "> velocity=<"
    _ <- optionMaybe $ char ' '
    velX <- intParser
    _ <- string ", "
    _ <- optionMaybe $ char ' '
    velY <- intParser
    _ <- char '>'

    return ((posX, posY), (velX, velY))

parseRow :: String -> (Position, Velocity)
parseRow s = case parse rowParser "" s of
    Right pvs -> pvs
    Left _ -> error "Parsing failed for one or more rows."

step :: [(Position, Velocity)] -> [(Position, Velocity)]
step = map (\(p, v) -> (Data.Bifunctor.bimap (fst p +) (snd p +) v, v))

showBoard :: [(Position, Velocity)] -> String
showBoard pvs = unlines rows
  where
    ps = map fst pvs
    pset = Set.fromList ps
    minX = minimum $ map fst ps
    maxX = maximum $ map fst ps
    minY = minimum $ map snd ps
    maxY = maximum $ map snd ps
    rows = map (\yi -> [if (x, y) `Set.member` pset then '#' else ' ' | x <- [minX .. maxX], y <- [yi]]) [minY .. maxY]

isSolved :: [(Position, Velocity)] -> Bool
isSolved pvs = maxY - minY < 10
  where
    ps = map fst pvs
    maxY = maximum $ map snd ps
    minY = minimum $ map snd ps

solution :: String -> String
solution s = showBoard . head $ dropWhile (not . isSolved) (iterate step . map parseRow $ lines s)
