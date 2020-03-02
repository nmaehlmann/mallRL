{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where
import Apecs hiding (Map, Set)
import qualified Apecs as Apecs
import Linear
import TileImage
import Position
import Renderer
import qualified SDL
import Data.Array
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Colors
import TileMap
import CDrawable
import Item
import Apecs.Experimental.Reactive

newtype CPosition = CPosition Position deriving Show
instance Component CPosition where type Storage CPosition = Apecs.Map CPosition

data CPlayer = CPlayer deriving Show
instance Component CPlayer where type Storage CPlayer = Unique CPlayer

instance Component CDrawable where type Storage CDrawable = Apecs.Map CDrawable

data CItem = CItem Item
instance Component CItem where type Storage CItem = Apecs.Map CItem

data CMap = CMap (Map Position [Entity])
instance Component CMap where type Storage CMap = Global CMap
instance Semigroup CMap where _ <> m = m
instance Monoid CMap where mempty = CMap Map.empty

data CSolid = CSolid
instance Component CSolid where type Storage CSolid = Apecs.Map CSolid

data CInventory = CInventory [Item]
instance Component CInventory where type Storage CInventory = Global CInventory
instance Semigroup CInventory where (CInventory l1) <> (CInventory l2) = CInventory $ l1 <> l2
instance Monoid CInventory where mempty = CInventory []

makeWorld "World" [''CPosition, ''CPlayer, ''CDrawable, ''CMap, ''CSolid, ''CItem]

destroyEntity :: Entity -> System' () 
destroyEntity e = destroy e (Proxy :: Proxy (CPosition, CPlayer, CDrawable, CSolid))

type System' a = System World a

initialize :: System' ()
initialize = do
    newEntity (CPlayer, CPosition (V2 0 0), dPlayer)
    mapM (\p -> newEntity (CSolid, p, dWall)) [CPosition (V2 x y) | x <- [5, 15, 25] , y <- [5..15]]
    newEntity (CItem Pizza, dPizza, CPosition (V2 4 19))
    return ()

step :: Float -> System' ()
step dT = do
    updateMap
    lift $ putStrLn $ "step " ++ show dT

updateMap :: System' ()
updateMap = do
    newMap <- fmap CMap $ cfold insertEntity Map.empty
    set global newMap

insertEntity :: Map Position [Entity] -> (CPosition, Entity) -> Map Position [Entity]
insertEntity newMap (CPosition p, Entity e) = Map.alter (addToSet e) p newMap
    where  
        addToSet e (Just s) = Just $ (Entity e) : s
        addToSet e Nothing  = Just $ [Entity e]


handleEvent :: SDL.EventPayload -> System' ()
handleEvent e = do
    whenKeyPressed SDL.ScancodeRight e  $ cmapM $ \(CPlayer, CPosition p) -> move right p
    whenKeyPressed SDL.ScancodeLeft e   $ cmapM $ \(CPlayer, CPosition p) -> move left p
    whenKeyPressed SDL.ScancodeUp e     $ cmapM $ \(CPlayer, CPosition p) -> move up p
    whenKeyPressed SDL.ScancodeDown e   $ cmapM $ \(CPlayer, CPosition p) -> move down p

move :: (Position -> Position) -> Position -> System' CPosition
move direction p = moveTo p (direction p)

left, right, up, down :: Position -> Position
left (V2 x y) = V2 (x - 1) y
right (V2 x y) = V2 (x + 1) y
up (V2 x y) = V2 x (y - 1)
down (V2 x y) = V2 x (y + 1)

moveTo :: Position -> Position -> System' CPosition
moveTo source target = do
    entitiesAtTarget <- entitiesAtPosition target
    targetBlocked <- elem True <$> mapM (\e -> exists e (Proxy :: Proxy CSolid)) entitiesAtTarget
    return $ CPosition $ if targetBlocked then source else target

-- interactWith :: Entity -> System' ()
-- interactWith e = do


entitiesAtPosition :: Position -> System' [Entity]
entitiesAtPosition pos = do
    (CMap m) <- get global
    return $ Map.findWithDefault [] pos m

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