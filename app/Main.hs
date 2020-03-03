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
    newEntity (CPlayer, CPosition (V2 0 0), dPlayer)
    mkShelf 5 5 7 Seaweed Pizza
    mkShelf 11 5 7 Bananas Pizza
    mkShelf 17 5 7 Fishsticks Fishsticks
    return ()

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

step :: Float -> System' ()
step dT = lift $ putStrLn $ "step " ++ show dT

handleEvent :: SDL.EventPayload -> System' ()
handleEvent e = do
    whenKeyPressed SDL.ScancodeRight e  $ cmapM $ \(CPlayer, CPosition p, e) -> move right p e
    whenKeyPressed SDL.ScancodeLeft e   $ cmapM $ \(CPlayer, CPosition p, e) -> move left p e
    whenKeyPressed SDL.ScancodeUp e     $ cmapM $ \(CPlayer, CPosition p, e) -> move up p e
    whenKeyPressed SDL.ScancodeDown e   $ cmapM $ \(CPlayer, CPosition p, e) -> move down p e

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
    \(CPlayer) (CItem i) -> destroyEntity itemEntity

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