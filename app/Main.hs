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
import Colors
import TileMap
import CDrawable
import Item
import World
import Interaction
import TerminalText
import MapGeneration
import Pathfinding
import Draw
import Control.Monad

initialize :: System' ()
initialize = do
    initializeMap
    let shoppingList = [Pizza, Seaweed, Bananas, Fishsticks]
    newEntity (CPlayer, CPosition (V2 1 1), dPlayer, CSolid, CInventory [], CName "You", CShoppingList shoppingList)
    newEntity (CBehaviour (Buy Seaweed), CPosition (V2 2 1), Drawable (charToGlyph 'K') black, CSolid, CInventory [], CName "Kunibert")
    newEntity (CBehaviour (Buy Pizza), CPosition (V2 3 1), Drawable (charToGlyph 'J') black, CSolid, CInventory [], CName "Jens")
    modify global $ appendAction Redisplay
    return ()

mkBorder :: System' ()
mkBorder = do
    let allPositions = [V2 x y | x <- [0..mapWidth - 1], y <- [0..mapHeight - 1]]
    let edgePositions = map (\(V2 x y) -> V2 (fromIntegral x) (fromIntegral y)) $ filter (\(V2 x y) -> x == 0 || y == 0 || x == mapWidth - 1 || y == mapHeight - 1) allPositions
    flip mapM_ edgePositions $ \p -> newEntity (CSolid, CPosition p, dWall)

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
handleAction _ = return ()

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
    whenKeyPressed SDL.ScancodeR e      $ do
        cmapM_ $ \(CPosition p, Entity e) -> destroyEntity (Entity e)
        initialize

appendAction :: Action -> CActions -> CActions
appendAction a (CActions as) = CActions $ as ++ [a]

move :: (Position -> Position) -> Position -> Entity -> System' CPosition
move direction p e = moveTo p (direction p) e

moveTo :: Position -> Position -> Entity -> System' CPosition
moveTo source target movingEntity = do
    entitiesAtTarget <- entitiesAtPosition target
    mapM (pickupItem movingEntity) entitiesAtTarget
    mapM (enterRooms movingEntity) entitiesAtTarget
    targetBlocked <- containsSolidEntity entitiesAtTarget
    return $ CPosition $ if targetBlocked then source else target

pickupItem :: Entity -> Entity -> System' ()
pickupItem activeEntity itemEntity = interaction activeEntity itemEntity $
    \(CInventory _, CName name) (CItem i) -> do
        modify activeEntity $ \(CInventory items) -> CInventory $ i : items
        isPlayer <- exists activeEntity (Proxy :: Proxy CPlayer)
        when isPlayer $ do
            logTxt $ FGText name (V3 255 255 0) <> FGText " picked up " white <> FGText (show i) (V3 255 0 0)
        destroyEntity itemEntity

enterRooms :: Entity -> Entity -> System' ()
enterRooms playerEntity itemEntity = interaction playerEntity itemEntity $
    \(CPlayer) rooms@(CIsInRoom _) -> set playerEntity rooms

whenKeyPressed :: SDL.Scancode -> SDL.EventPayload -> System' () -> System' ()
whenKeyPressed s e sys = if (isKeyPressed s e) then sys else return ()

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

logTxt :: TerminalText -> System' ()
logTxt txt = modify global $ \(CLog txts) -> CLog $ txt : txts