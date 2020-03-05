{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia                #-}
module Main where
import Apecs hiding (Map, Set)
import Linear
import TileImage
import Position
import Renderer
import qualified SDL
import Data.Array
import Colors
import TileMap
import CDrawable
import Item
import Apecs.Experimental.Reactive
import Control.Monad
import World
import Interaction
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Graph.AStar
import TerminalText
import UI

initialize :: System' ()
initialize = do
    mkBorder
    let shoppingList = [Pizza, Seaweed, Bananas, Fishsticks]
    newEntity (CPlayer, CPosition (V2 1 1), dPlayer, CSolid, CInventory [], CName "You", CShoppingList shoppingList)
    newEntity (CBehaviour (Buy Seaweed), CPosition (V2 2 1), Drawable (charToGlyph 'K') black, CSolid, CInventory [], CName "Kunibert")
    newEntity (CBehaviour (Buy Pizza), CPosition (V2 3 1), Drawable (charToGlyph 'J') black, CSolid, CInventory [], CName "Jens")
    mkShelf 5 5 7 Seaweed Pizza
    mkShelf 11 5 7 Bananas Pizza
    mkShelf 17 5 7 Fishsticks Fishsticks
    return ()

mkBorder :: System' ()
mkBorder = do
    let allPositions = [V2 x y | x <- [0..mapWidth - 1], y <- [0..mapHeight - 1]]
    let edgePositions = map (\(V2 x y) -> V2 (fromIntegral x) (fromIntegral y)) $ filter (\(V2 x y) -> x == 0 || y == 0 || x == mapWidth - 1 || y == mapHeight - 1) allPositions
    flip mapM_ edgePositions $ \p -> newEntity (CSolid, CPosition p, dWall)

mkShelf :: Int -> Int -> Int -> Item -> Item -> System' ()
mkShelf x y l leftItem rightItem = do
    flip mapM_ [V2 x ys | ys <- [y + 1 .. y + l - 2] ] $ \p -> do
        -- shelf middle
        newEntity (CSolid, CPosition p, dWall)
        
        -- shelf left
        newEntity (CSolid, CPosition (left p), dShelf)
        newEntity (CItem leftItem, CPosition (left p), lookupItemDrawable leftItem)
        
        -- shelf right
        newEntity (CSolid, CPosition (right p), dShelf)
        newEntity (CItem rightItem, CPosition (right p), lookupItemDrawable rightItem)

    flip mapM_ [V2 xs y | xs <- [x - 1 .. x + 1] ] $ \p -> do
        -- shelf north bound
        newEntity (CSolid, CPosition p, dShelfNorth)

    flip mapM_ [V2 xs (y + l - 1) | xs <- [x - 1 .. x + 1] ] $ \p -> do
        -- shelf south bound
        newEntity (CSolid, CPosition p, dShelfSouth)

stepDuration = 0.6

step :: Float -> System' Bool
step dT = do
    actionsDirty <- handleActions
    stepTime dT
    return actionsDirty

handleActions :: System' Bool
handleActions = do
    action <- pollAction
    case action of
        (Just a) -> handleAction a >> turn >> return True
        Nothing -> return False

handleAction :: Action -> System' ()
handleAction (Move d) = cmapM (movePlayer d)

turn :: System' ()
turn = do
    cmapM $ \ (CPosition position, CBehaviour (Buy item), e) -> do
        path <- pathToItem position item
        case path of
            (Just (nextPosition : _)) -> moveTo position nextPosition e
            _ -> return $ CPosition position
        

pollAction :: System' (Maybe Action)
pollAction = do
    actions <- get global
    case actions of
        (CActions []) -> return Nothing
        (CActions (a:as)) -> do
            set global $ CActions as 
            return $ Just a

stepTime :: Float -> System' ()
stepTime dT = do
    (CTime t) <- get global
    let newTime = t + dT
    let stepDone = stepDuration <= newTime
    if stepDone
        then set global $ CTime $ newTime - stepDuration
        else set global $ CTime newTime

movePlayer :: Direction -> (CPlayer, CPosition, Entity) -> System' CPosition
movePlayer d (_, (CPosition p), e) = move (dirToFun d) p e

dirToFun :: Direction -> (Position -> Position)
dirToFun DirLeft = left
dirToFun DirRight = right
dirToFun DirUp = up
dirToFun DirDown = down

handleEvent :: SDL.EventPayload -> System' ()
handleEvent e = do
    whenKeyPressed SDL.ScancodeRight e  $ modify global $ appendAction $ Move DirRight
    whenKeyPressed SDL.ScancodeLeft e   $ modify global $ appendAction $ Move DirLeft
    whenKeyPressed SDL.ScancodeUp e     $ modify global $ appendAction $ Move DirUp
    whenKeyPressed SDL.ScancodeDown e   $ modify global $ appendAction $ Move DirDown

appendAction :: Action -> CActions -> CActions
appendAction a (CActions as) = CActions $ as ++ [a]

move :: (Position -> Position) -> Position -> Entity -> System' CPosition
move direction p e = moveTo p (direction p) e

left, right, up, down :: Position -> Position
left (V2 x y) = V2 (x - 1) y
right (V2 x y) = V2 (x + 1) y
up (V2 x y) = V2 x (y - 1)
down (V2 x y) = V2 x (y + 1)

moveTo :: Position -> Position -> Entity -> System' CPosition
moveTo source target movingEntity = do
    entitiesAtTarget <- entitiesAtPosition target
    mapM (pickupItem movingEntity) entitiesAtTarget
    targetBlocked <- containsSolidEntity entitiesAtTarget
    return $ CPosition $ if targetBlocked then source else target

containsSolidEntity :: [Entity] -> System' Bool
containsSolidEntity es = elem True <$> mapM (\e -> exists e (Proxy :: Proxy CSolid)) es

pickupItem :: Entity -> Entity -> System' ()
pickupItem playerEntity itemEntity = interaction playerEntity itemEntity $
    \(CInventory _, CName name) (CItem i) -> do
        modify playerEntity $ \(CInventory items) -> CInventory $ i : items
        logTxt $ (FGText name (V3 255 255 0)) <> (FGText " picked up " white) <> (FGText (show i) (V3 255 0 0))
        destroyEntity itemEntity

entitiesAtPosition :: Position -> System' [Entity]
entitiesAtPosition pos = withReactive $ ixLookup (CPosition pos)

whenKeyPressed :: SDL.Scancode -> SDL.EventPayload -> System' () -> System' ()
whenKeyPressed s e sys = if (isKeyPressed s e) then sys else return ()

draw :: System' TileImage
draw = cfold cDrawDrawable testMap >>= drawUI

cDrawDrawable :: TileImage -> (CPosition, CDrawable) -> TileImage
cDrawDrawable tm (CPosition pos, drawable) = drawDrawable tm (pos, drawable)

isKeyPressed :: SDL.Scancode -> SDL.EventPayload -> Bool
isKeyPressed scancode (SDL.KeyboardEvent e) = pressed && justPressed && rightKey
    where 
        pressed = SDL.keyboardEventKeyMotion e == SDL.Pressed
        justPressed = SDL.keyboardEventRepeat e == False
        rightKey = scancode == (SDL.keysymScancode (SDL.keyboardEventKeysym e ))
isKeyPressed _ _ = False

main :: IO ()
main = do
    w <- initWorld
    runWith w $ do
        initialize
        wNew <- ask
        lift $ play wNew draw handleEvent step

pathToItem :: Position -> Item -> System' (Maybe [Position])
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

neighbours :: Position -> Position -> System' (HashSet Position) 
neighbours goal p = do
    ps <- mapM (filterFreePosition goal) $ [left, right, up, down] <*> [p]
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



logTxt :: TerminalText -> System' ()
logTxt txt = modify global $ \(CLog txts) -> CLog $ txt : txts