module Day15.Part01 (solution) where

import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

type Coord = (Int, Int)

data UnitType
    = Goblin
    | Elf
    deriving (Eq, Show)
data Unit = Unit {unitTypeU :: UnitType, hpU :: Int}
data World = World {wallsW :: Set Coord, unitsW :: Map Coord Unit}

parseInput :: String -> World
parseInput s = world
  where
    -- label coords (y, x) instead of (x, y) so standard sort is reading order
    coorded :: [(Coord, Char)]
    coorded =
        concatMap
            (\(y, r) -> [((y, x), c) | (x, c) <- zip [0 ..] r])
            . zip [0 ..]
            $ lines s
    getType 'G' = Goblin
    getType 'E' = Elf
    getType _ = error "Invalid unit type."

    world =
        World
            { wallsW = S.fromList . map fst $ filter ((== '#') . snd) coorded
            , unitsW =
                M.fromList $
                    map (\(p, c) -> (p, Unit (getType c) 200)) $
                        filter ((`elem` "GE") . snd) coorded
            }

enemyOf :: UnitType -> UnitType
enemyOf Goblin = Elf
enemyOf Elf = Goblin

neighbors :: Coord -> Set Coord
neighbors (x, y) = S.fromList [(x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1)]

closest :: Set Coord -> Set Coord -> Coord -> Maybe Coord
closest walls goals = go walls . S.singleton
  where
    go visited queue
        | S.null queue = Nothing
        | reached <- S.intersection goals queue, not $ S.null reached = S.lookupMin reached
        | otherwise =
            let visited' = S.union visited queue
             in go visited' $ S.unions (neighbors <$> S.toList queue) S.\\ visited

tick :: Set Coord -> Map Coord Unit -> (Map Coord Unit, Bool)
tick walls = go M.empty
  where
    go :: Map Coord Unit -> Map Coord Unit -> (Map Coord Unit, Bool)
    go past current
        | M.null current = (past, False)
        | otherwise =
            let ((pos, unit), future) = case M.minViewWithKey current of
                    Just res -> res
                    Nothing -> error "Unreachable state"
                otherUnits = M.union past future
                walls' = S.union walls $ M.keysSet otherUnits
                enemies = M.filter ((== enemyOf (unitTypeU unit)) . unitTypeU) otherUnits
                orthogonalEnemyPos = neighbors pos `S.intersection` M.keysSet enemies
                enemyRanges = S.unions (neighbors <$> M.keys enemies) S.\\ walls'
                mgoal =
                    if S.null orthogonalEnemyPos
                        then closest walls' enemyRanges pos
                        else Nothing
                pos' = case mgoal of
                    Just goal | Just m <- closest walls' (neighbors pos) goal -> m
                    _ -> pos
                orthogonalEnemies = M.restrictKeys enemies $ neighbors pos'
             in if M.null enemies
                    then
                        (M.insert pos unit otherUnits, True)
                    else
                        if M.null orthogonalEnemies
                            then
                                go (M.insert pos' unit past) future
                            else case sortOn (hpU . snd) $ M.toList orthogonalEnemies of
                                [] -> error "Unreachable state"
                                (target, _) : _ -> do
                                    past' <- M.insert pos' unit <$> M.alterF attack target past
                                    future' <- M.alterF attack target future
                                    go past' future'
      where
        attack :: (Monad f) => Maybe Unit -> f (Maybe Unit)
        attack (Just Unit{unitTypeU = unitType, hpU = hp})
            | hp > 3 = return . Just $ Unit{unitTypeU = unitType, hpU = hp - 3}
        attack _ = return Nothing

outcome :: World -> Int
outcome World{wallsW = walls, unitsW = units} = go 0 units
  where
    go :: Int -> Map Coord Unit -> Int
    go rounds units' = case tick walls units' of
        (units'', True) -> rounds * sum (map hpU $ M.elems units'')
        (units'', False) -> go (rounds + 1) units''

solution :: String -> Int
solution s = outcome world
  where
    world = parseInput s
