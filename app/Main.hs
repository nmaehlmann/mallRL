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

newtype CPosition = CPosition Position deriving Show
instance Component CPosition where type Storage CPosition = Apecs.Map CPosition

data CPlayer = CPlayer deriving Show
instance Component CPlayer where type Storage CPlayer = Unique CPlayer

data CMap = CMap (Map Position [Entity])
instance Component CMap where type Storage CMap = Global CMap
instance Semigroup CMap where _ <> m = m
instance Monoid CMap where mempty = CMap Map.empty

data CDrawable = Drawable Glyph Color | DrawableBG Glyph Color Color
instance Component CDrawable where type Storage CDrawable = Apecs.Map CDrawable

data CSolid = CSolid
instance Component CSolid where type Storage CSolid = Apecs.Map CSolid

makeWorld "World" [''CPosition, ''CPlayer, ''CDrawable, ''CMap, ''CSolid]

type System' a = System World a

playerGlyph :: Glyph
playerGlyph = V2 2 0

playerColor :: Color
playerColor = V3 0 0 0

wallGlyph :: Glyph
wallGlyph = V2 11 13

wallColor :: Color
wallColor = V3 0 0 0

initialize :: System' ()
initialize = do
    newEntity (CPlayer, CPosition (V2 0 0), Drawable playerGlyph playerColor)
    mapM (\p -> newEntity (CSolid, p, Drawable wallGlyph wallColor)) [CPosition (V2 x y) | x <- [5, 15, 25] , y <- [5..15]]
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
move f p = moveTo p (f p)

left, right, up, down :: Position -> Position
left (V2 x y) = V2 (x - 1) y
right (V2 x y) = V2 (x + 1) y
up (V2 x y) = V2 x (y - 1)
down (V2 x y) = V2 x (y + 1)

moveTo :: Position -> Position -> System' CPosition
moveTo source target = do
    (CMap m) <- get global
    let entitiesAtTarget = Map.findWithDefault [] target m
    targetBlocked <- elem True <$> mapM (\e -> exists e (Proxy :: Proxy CSolid)) entitiesAtTarget
    return $ CPosition $ if targetBlocked then source else target


-- forall w m c. Get w m c => Entity -> Proxy c -> SystemT w m Bool
-- hasTag :: forall w m c. c -> Entity -> SystemT w m Bool
-- hasTag _ e = exists e Proxy


-- getEntitiesForPosition :: Position -> System' (Set Entity)
-- getEntitiesForPosition p = cm

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

-- newtype Screen = Screen TileImage
-- instance Semigroup Score where (<>) = (+)
-- instance Monoid Score where mempty = 0
-- instance Component Score where type Storage Score = Global Score
