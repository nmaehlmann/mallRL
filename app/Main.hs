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


initialize :: System' ()
initialize = do
    mkBorder
    newEntity (CPlayer, CPosition (V2 1 1), dPlayer, CInventory [])
    newEntity (CShoppingCart, CPosition (V2 2 2), dShoppingCart)
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
        newEntity (CItem leftItem, CPosition (right p), lookupItemDrawable rightItem)

    flip mapM_ [V2 xs y | xs <- [x - 1 .. x + 1] ] $ \p -> do
        -- shelf north bound
        newEntity (CSolid, CPosition p, dShelfNorth)

    flip mapM_ [V2 xs (y + l - 1) | xs <- [x - 1 .. x + 1] ] $ \p -> do
        -- shelf south bound
        newEntity (CSolid, CPosition p, dShelfSouth)

stepDuration = 0.6

step :: Float -> System' ()
step dT = do
    (CTime t) <- get global
    let newTime = t + dT
    let stepDone = stepDuration <= newTime
    if stepDone
        then set global $ CTime $ newTime - stepDuration
        else set global $ CTime newTime
    when stepDone $ do
        cmapM moveDirection
        cmapM_ $ \(CPlayer, CPosition pPlayer, CDirection d) -> cmap $ \(CShoppingCart, CPosition pCart) -> CPosition $ (dirToFun d) pPlayer

moveDirection :: (CDirection, CPosition, Entity) -> System' CPosition
moveDirection (CDirection d, (CPosition p), e) = move (dirToFun d) p e

dirToFun :: Direction -> (Position -> Position)
dirToFun DirLeft = left
dirToFun DirRight = right
dirToFun DirUp = up
dirToFun DirDown = down

handleEvent :: SDL.EventPayload -> System' ()
handleEvent e = do
    whenKeyPressed SDL.ScancodeRight e  $ cmap $ \CPlayer -> CDirection DirRight
    whenKeyPressed SDL.ScancodeLeft e   $ cmap $ \CPlayer -> CDirection DirLeft
    whenKeyPressed SDL.ScancodeUp e     $ cmap $ \CPlayer -> CDirection DirUp
    whenKeyPressed SDL.ScancodeDown e   $ cmap $ \CPlayer -> CDirection DirDown

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
    targetBlocked <- elem True <$> mapM (\e -> exists e (Proxy :: Proxy CSolid)) entitiesAtTarget
    return $ CPosition $ if targetBlocked then source else target

pickupItem :: Entity -> Entity -> System' ()
pickupItem playerEntity itemEntity = interaction playerEntity itemEntity $
    \(CPlayer) (CItem i) -> do
        modify playerEntity $ \(CInventory items) -> CInventory $ i : items
        destroyEntity itemEntity

entitiesAtPosition :: Position -> System' [Entity]
entitiesAtPosition pos = withReactive $ ixLookup (CPosition pos)

whenKeyPressed :: SDL.Scancode -> SDL.EventPayload -> System' () -> System' ()
whenKeyPressed s e sys = if (isKeyPressed s e) then sys else return ()

draw :: System' TileImage
draw = cfold drawDrawable testMap

drawDrawable :: TileImage -> (CPosition, CDrawable) -> TileImage
drawDrawable (TileImage tm) ((CPosition pos, Drawable glyph color)) = 
    let (Tile _ _ bgColor) = tm ! pos
    in TileImage $ tm // [(pos, Tile glyph color bgColor)]
drawDrawable (TileImage tm) ((CPosition pos, DrawableBG glyph color bgColor)) = TileImage $ tm // [(pos, Tile glyph color bgColor)]

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