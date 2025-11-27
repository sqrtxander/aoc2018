module Day10.Part02 (solution) where

import qualified Data.Bifunctor
import Data.Functor.Identity
import Data.List (findIndex)
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

isSolved :: [(Position, Velocity)] -> Bool
isSolved pvs = maxY - minY < 10
  where
    ps = map fst pvs
    maxY = maximum $ map snd ps
    minY = minimum $ map snd ps

solution :: String -> Int
solution s =
    case findIndex isSolved (iterate step . map parseRow $ lines s) of
        Just i -> i
        Nothing -> error "Solution not found"
