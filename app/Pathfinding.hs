module Pathfinding where
import Apecs hiding (Map, Set)
import Linear
import Position
import Item
import World
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Graph.AStar
import Control.Monad

pathToItem :: Position -> Item -> System' (Maybe [Position])
-- pathToItem p i = return Nothing
pathToItem currentPosition item = do
    itemPositions <- flip cfold [] $ \l (CItem i, CPosition p) -> if item == i 
            then p:l
            else l
    case itemPositions of
        [] -> return Nothing
        (goal : _) -> do
            aStarM 
                (neighbours goal)
                distanceBetweenNeighbours
                (heuristicDistanceToGoal goal)
                (\p -> return (p == goal))
                (return currentPosition)

pathToPosition :: Position -> Position -> System' (Maybe [Position])
pathToPosition currentPosition goal = aStarM 
    (neighbours goal)
    distanceBetweenNeighbours
    (heuristicDistanceToGoal goal)
    (\p -> return (p == goal))
    (return currentPosition)

positionValid :: Position -> Bool
positionValid (V2 x y) = x < positionMaxX && y < positionMaxY && x >= 0 && y >= 0

itemPositions :: Item -> System' [Position]
itemPositions item = flip cfold [] $ \l (CItem i, CPosition p) -> if item == i then p:l else l

isItemOnPosition :: Item -> Position -> System' Bool
isItemOnPosition item pos = do
    entities <- entitiesAtPosition pos
    not <$> null <$> filterM (\e -> e `isItem` item) entities

isItem :: Entity -> Item -> System' Bool
isItem e item = do
    isAnItem <- exists e (Proxy :: Proxy CItem)
    if isAnItem then do
        (CItem i) <- get e
        return $ i == item
    else return False

neighbours :: Position -> Position -> System' (HashSet Position) 
neighbours goal p = do
    ps <- mapM (filterFreePosition goal) $ filter positionValid $ [left, right, up, down] <*> [p]
    return $ HashSet.unions ps

distanceBetweenNeighbours :: Position -> Position -> System' Float
distanceBetweenNeighbours _ _ = return 1

heuristicDistanceToGoal :: Position -> Position -> System' Float
heuristicDistanceToGoal goal p = return $ distance (fmap (fromIntegral) goal) (fmap (fromIntegral) p)

filterFreePosition :: Position -> Position -> System' (HashSet Position) 
filterFreePosition goal target = if goal == target 
    then return $ HashSet.singleton target
    else do
        entitiesAtTarget <- entitiesAtPosition target
        targetBlocked <- containsSolidEntity entitiesAtTarget
        return $ if targetBlocked then HashSet.empty else HashSet.singleton target

containsSolidEntity :: [Entity] -> System' Bool
containsSolidEntity es = elem True <$> mapM (\e -> exists e (Proxy :: Proxy CSolid)) es