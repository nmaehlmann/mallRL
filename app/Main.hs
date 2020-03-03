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


destroyEntity :: Entity -> System' () 
destroyEntity e = destroy e (Proxy :: Proxy (CPosition, CPlayer, CDrawable, CSolid))

initialize :: System' ()
initialize = do
    newEntity (CPlayer, CPosition (V2 0 0), dPlayer)
    mapM (\p -> newEntity (CSolid, p, dWall)) [CPosition (V2 x y) | x <- [5, 15, 25] , y <- [5..15]]
    newEntity (CItem Pizza, dPizza, CPosition (V2 4 6))
    newEntity (CItem Seaweed, dSeaweed, CPosition (V2 4 7))
    newEntity (CItem Seaweed, dSeaweed, CPosition (V2 4 8))
    return ()

step :: Float -> System' ()
step dT = do
    pickupItems
    destroyInteractions
    lift $ putStrLn $ "step " ++ show dT

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
    mapM (markInteraction movingEntity) entitiesAtTarget
    targetBlocked <- elem True <$> mapM (\e -> exists e (Proxy :: Proxy CSolid)) entitiesAtTarget
    return $ CPosition $ if targetBlocked then source else target

pickupItems :: System' ()
pickupItems = do
    cmapM_ $ \(CPlayer, i1) ->
        cmapM_ $ \(CItem i, i2, itemEntity) -> do
            lift $ putStrLn $ show i1
            lift $ putStrLn $ show i2
            when (sameInteraction i1 i2) $ do
                destroyEntity itemEntity

destroyInteractions :: System' ()
destroyInteractions = cmap $ \(CInteraction i) -> Nothing :: Maybe CInteraction

sameInteraction :: (CInteraction, Entity) -> (CInteraction, Entity) -> Bool
sameInteraction (CInteraction iE1, e1) (CInteraction iE2, e2) = iE1 == e2 && iE2 == e1

markInteraction :: Entity -> Entity -> System' ()
markInteraction e1 e2 = do
    e1 $= CInteraction e2
    e2 $= CInteraction e1

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