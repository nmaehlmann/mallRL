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
import Apecs
import Linear
import TileImage
import Position
import Renderer
import qualified SDL
import Data.Array

newtype CPosition = CPosition Position deriving Show
instance Component CPosition where type Storage CPosition = Map CPosition

data CPlayer = CPlayer deriving Show
instance Component CPlayer where type Storage CPlayer = Unique CPlayer

data CDrawable = Drawable Glyph Color | DrawableBG Glyph Color Color
instance Component CDrawable where type Storage CDrawable = Map CDrawable

makeWorld "World" [''CPosition, ''CPlayer, ''CDrawable]

type System' a = System World a

playerGlyph :: Glyph
playerGlyph = V2 2 0

playerColor :: Color
playerColor = V3 0 0 0

initialize :: System' ()
initialize = do
    playerEty <- newEntity (CPlayer, CPosition (V2 0 0), Drawable playerGlyph playerColor)
    return ()

step :: Float -> System' ()
step dT = lift $ putStrLn $ "step " ++ show dT

handleEvent :: SDL.EventPayload -> System' ()
handleEvent e = do
    whenKeyPressed SDL.ScancodeRight e $ cmap (\(CPosition (V2 x y)) -> CPosition (V2 (x + 1) y))
    whenKeyPressed SDL.ScancodeLeft e $ cmap (\(CPosition (V2 x y)) -> CPosition (V2 (x - 1) y))
    whenKeyPressed SDL.ScancodeUp e $ cmap (\(CPosition (V2 x y)) -> CPosition (V2 x (y - 1)))
    whenKeyPressed SDL.ScancodeDown e $ cmap (\(CPosition (V2 x y)) -> CPosition (V2 x (y + 1)))
    return ()

whenKeyPressed :: SDL.Scancode -> SDL.EventPayload -> System' () -> System' ()
whenKeyPressed s e sys = if (isKeyPressed s e) then sys else return ()

draw :: System' TileImage
draw = cfold drawDrawable testMap 

drawDrawable :: TileImage -> (CPosition, CDrawable) -> TileImage
drawDrawable (TileImage tm) ((CPosition pos, Drawable glyph color)) = 
    let (Tile _ _ bgColor) = tm ! pos
    in TileImage $ tm // [(pos, Tile playerGlyph color bgColor)]
drawDrawable (TileImage tm) ((CPosition pos, DrawableBG glyph color bgColor)) = TileImage $ tm // [(pos, Tile playerGlyph color bgColor)]

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
