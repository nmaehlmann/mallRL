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
import Position
import Renderer
import qualified SDL
import Colors
import CDrawable
import Item
import World
import Interaction
import TerminalText
import MapGeneration
import Pathfinding
import Draw
import Control.Monad
import Data.Maybe
import RandomUtility
import Room
import Data.List

initialize :: System' ()
initialize = do
    set global $ Running 0
    welcomeMessage
    (mallRoom, carComponents) <- initializeMap
    set global $ CMallRoom mallRoom
    mkBorder mallRoom

    (CCar playerCarPosition) <- evalRandom $ pickRandom carComponents
    let playerPosition = right $ carDoorPosition playerCarPosition
    let shoppingList = [Pizza, Seaweed, Bananas, Fishsticks, Bananas, Bananas]
    newEntity (CPlayer, CPosition playerPosition, CSolid, CInventory [], CName "You", CShoppingList shoppingList, (CIsInRoom [], COwnsCar playerCarPosition))

    flip mapM_ carComponents $ \(CCar carPosition) -> do
        when (carPosition /= playerCarPosition) $ do
            let position = right $ carDoorPosition carPosition
            newEntity (CBehaviour Deciding, CPosition position, Drawable (charToGlyph 'B') npcColor, CSolid, CInventory [], CShoppingList shoppingList, CName "Kunibert", COwnsCar carPosition)
            return ()
            
    modify global $ appendAction Redisplay
    return ()


carDoorPosition :: Position -> Position
carDoorPosition carPosition = carPosition + V2 2 4

mkBorder :: Room -> System' ()
mkBorder (Room x y w h) = do
    let borderX = x + w + 5
    let allPositions = [V2 x y | x <- [0 .. borderX], y <- [0 .. positionMaxY]]
    let edgePositions = filter (\(V2 x y) -> x == 0 || y == 0 || x == borderX || y == positionMaxY) allPositions
    flip mapM_ edgePositions $ \p -> newEntity (CSolid, CPosition p, Drawable (charToGlyph '#') white)

stepDuration = 0.6

step :: Float -> System' Bool
step dT = do
    actionsDirty <- handleActions
    indoorOutdoor
    return actionsDirty

indoorOutdoor :: System' ()
indoorOutdoor = do
    (CMallRoom mallRoom) <- get global
    cmap $ \(CPlayer, CPosition pos) -> if (containsPosition pos mallRoom)
        then dPlayerIndoors
        else dPlayerOutdoors

handleActions :: System' Bool
handleActions = do
    action <- pollAction
    case action of
        (Just a) -> handleAction a >> turn >> return True
        Nothing -> return False

handleAction :: Action -> System' ()
handleAction (Move d) = cmapM (movePlayer d)
handleAction _ = return ()

whenGameIsRunning :: System' () -> System' ()
whenGameIsRunning s = do
    isRunning <- (/= Stopped) <$> get global
    when isRunning s

turn :: System' ()
turn = whenGameIsRunning $ do
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

movePlayer :: Direction -> (CPlayer, CPosition, Entity) -> System' CPosition
movePlayer d (_, (CPosition p), e) = move (dirToFun d) p e

dirToFun :: Direction -> (Position -> Position)
dirToFun DirLeft = left
dirToFun DirRight = right
dirToFun DirUp = up
dirToFun DirDown = down

handleEvent :: SDL.EventPayload -> System' ()
handleEvent e = do
    whenGameIsRunning $ do
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
    mapM (enterCar movingEntity) entitiesAtTarget
    targetBlocked <- containsSolidEntity entitiesAtTarget
    when (not targetBlocked) $ mapM_ (enterRooms movingEntity) entitiesAtTarget
    let newPos = CPosition $ if targetBlocked then source else target
    return (newPos, pickedupItems)

enterCar :: Entity -> Entity -> System' ()
enterCar playerEntity itemEntity = do
    interaction_ playerEntity itemEntity $ \(CPlayer, COwnsCar ownedCarPosition) (CCar carPosition, CPosition touchedPosition) -> do
        if ownedCarPosition /= carPosition then
            logTxtS "This is not your car."
        else if touchedPosition == carDoorPosition carPosition
            then logTxtS "First shop before you enter."
            else logTxtS "Go to the driver's door."

pickupItem :: Entity -> Entity -> System' (Maybe Item)
pickupItem activeEntity itemEntity = do
    interaction activeEntity itemEntity $ \(CInventory currentInventory, CName name, CShoppingList sl) (CItem i) -> do
        let newInventory = i : currentInventory
        set activeEntity $ CInventory newInventory
        isPlayer <- exists activeEntity (Proxy :: Proxy CPlayer)
        when isPlayer $ do
            logTxt $ FGText name (V3 255 255 0) <> FGText " picked up " white <> FGText (show i) (V3 255 0 0)
            let allItemsBought = null $ sl \\ newInventory
            if allItemsBought
                then logTxtS "You got everything you need. Go to your car now."
                else if length newInventory >= (length sl) + shoppingListBuffer
                    then looseFullInventory
                    else return ()
        destroyEntity itemEntity
        return i

looseFullInventory :: System' ()
looseFullInventory = logTxtS "You bought way too much." >> loose

loose :: System' ()
loose = do
    logTxtS "You loose. Press [r] to restart."
    set global Stopped

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

welcomeMessage :: System' ()
welcomeMessage = do
    logTxtS $ "Welcome to mallRL!"
    logTxtS $ "Your goal is to buy every item from your shopping list."
    logTxtS $ "Use WASD or arrow keys to move and have fun shopping."
