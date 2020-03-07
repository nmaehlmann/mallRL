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
import Car
import Data.Maybe

initialize :: System' ()
initialize = do
    initializeMap
    mkBorder
    let shoppingList = [Pizza, Seaweed, Bananas, Fishsticks]
    newEntity (CPlayer, CPosition (V2 1 1), dPlayer, CSolid, CInventory [], CName "You", CShoppingList shoppingList, CIsInRoom [])
    newEntity (CBehaviour Deciding, CPosition (V2 2 1), Drawable (charToGlyph 'K') black, CSolid, CInventory [], CShoppingList shoppingList, CName "Kunibert")
    newEntity (CBehaviour Deciding, CPosition (V2 3 1), Drawable (charToGlyph 'J') black, CSolid, CInventory [], CShoppingList shoppingList, CName "Jens")
    newEntity (CBehaviour Deciding, CPosition (V2 4 1), Drawable (charToGlyph 'J') black, CSolid, CInventory [], CShoppingList shoppingList, CName "Jens")
    newEntity (CBehaviour Deciding, CPosition (V2 5 1), Drawable (charToGlyph 'J') black, CSolid, CInventory [], CShoppingList shoppingList, CName "Jens")
    newEntity (CBehaviour Deciding, CPosition (V2 6 1), Drawable (charToGlyph 'J') black, CSolid, CInventory [], CShoppingList shoppingList, CName "Jens")
    newEntity (CBehaviour Deciding, CPosition (V2 7 1), Drawable (charToGlyph 'J') black, CSolid, CInventory [], CShoppingList shoppingList, CName "Jens")
    newEntity (CBehaviour Deciding, CPosition (V2 8 1), Drawable (charToGlyph 'J') black, CSolid, CInventory [], CShoppingList shoppingList, CName "Jens")
    newEntity (CBehaviour Deciding, CPosition (V2 9 1), Drawable (charToGlyph 'J') black, CSolid, CInventory [], CShoppingList shoppingList, CName "Jens")
    modify global $ appendAction Redisplay
    return ()

mkBorder :: System' ()
mkBorder = do
    let allPositions = [V2 x y | x <- [0..positionMaxX], y <- [0..positionMaxY]]
    let edgePositions = filter (\(V2 x y) -> x == 0 || y == 0 || x == positionMaxX || y == positionMaxY) allPositions
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
    cmapM $ \ (CPosition position, CBehaviour behaviour, CShoppingList toBuy, e) -> CBehaviour <$> case behaviour of 
        (Buy item []) -> return Deciding -- This should never happen
        currentBehaviour@(Buy item path@(nextStep : nextSteps)) -> do
            let targetPosition = last path
            itemStillThere <- isItemOnPosition item targetPosition
            if itemStillThere then do
                ((CPosition movedPosition), pickedupItems) <- moveTo position nextStep e
                set e (CPosition movedPosition)
                if item `elem` pickedupItems 
                then return Deciding -- Succesfully bought item
                else if movedPosition == position 
                    then do -- Path is blocked, recalculate
                        recalculate <- evalRandom $ chance 10
                        if recalculate then do
                            maybePath <- pathToPosition position targetPosition
                            return $ case maybePath of
                                (Just path) -> Buy item path
                                Nothing -> currentBehaviour
                        else return currentBehaviour
                    else return $ Buy item nextSteps
            else return Deciding -- Item is gone
        Deciding -> do
            itemToBuy <- evalRandom $ pickRandom toBuy
            itemLocations <- itemPositions itemToBuy
            if null itemLocations
            then return Deciding
            else do
                selectedLocation <- evalRandom $ pickRandom itemLocations
                maybePath <- pathToPosition position selectedLocation
                return $ case maybePath of
                    (Just path) -> Buy itemToBuy path
                    Nothing -> Deciding

logTxtS s = logTxt $ FGText s white

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
move direction p e = fst <$> moveTo p (direction p) e

moveTo :: Position -> Position -> Entity -> System' (CPosition, [Item])
moveTo source target movingEntity = do
    entitiesAtTarget <- entitiesAtPosition target
    pickedupItems <- catMaybes <$> mapM (pickupItem movingEntity) entitiesAtTarget
    targetBlocked <- containsSolidEntity entitiesAtTarget
    when (not targetBlocked) $ mapM_ (enterRooms movingEntity) entitiesAtTarget
    let newPos = CPosition $ if targetBlocked then source else target
    return (newPos, pickedupItems)

pickupItem :: Entity -> Entity -> System' (Maybe Item)
pickupItem activeEntity itemEntity = do
    interaction activeEntity itemEntity $ \(CInventory _, CName name) (CItem i) -> do
        modify activeEntity $ \(CInventory items) -> CInventory $ i : items
        isPlayer <- exists activeEntity (Proxy :: Proxy CPlayer)
        when isPlayer $ do
            logTxt $ FGText name (V3 255 255 0) <> FGText " picked up " white <> FGText (show i) (V3 255 0 0)
        destroyEntity itemEntity
        return i

enterRooms :: Entity -> Entity -> System' ()
enterRooms playerEntity itemEntity = interaction_ playerEntity itemEntity $
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