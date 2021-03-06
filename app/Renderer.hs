{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Renderer where

import Control.Monad
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import Data.Array ((!))
import qualified Data.Array as Array
import Position
import Apecs hiding (($=))
import Control.Concurrent

import TileImage
import TileMap
import Colors

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (mapWidth * tileSize, mapHeight * tileSize)

data Texture = Texture SDL.Texture (V2 CInt)

createBlank :: SDL.Renderer -> V2 CInt -> SDL.TextureAccess -> IO Texture
createBlank r size access = Texture <$> SDL.createTexture r SDL.RGBA8888 access size <*> pure size

setAsRenderTarget :: SDL.Renderer -> Maybe Texture -> IO ()
setAsRenderTarget r Nothing = SDL.rendererRenderTarget r $= Nothing
setAsRenderTarget r (Just (Texture t _)) = SDL.rendererRenderTarget r $= Just t

loadTexture :: SDL.Renderer -> FilePath -> IO Texture
loadTexture r filePath = do
    surface <-  SDL.loadBMP filePath
    size <- SDL.surfaceDimensions surface
    t <- SDL.createTextureFromSurface r surface
    SDL.freeSurface surface
    return (Texture t size)

renderTileMap :: SDL.Renderer -> Texture -> TileImage -> TileImage -> IO ()
renderTileMap r t (TileImage previous) (TileImage arr) = mapM st (Array.range (Array.bounds arr)) >> return ()
    where st pos = do
            let tileOld = previous ! pos
            let tileNew = arr ! pos
            unless (tileOld == tileNew) $ do
                renderTile r t pos tileNew
    
setTextureColor :: Texture -> Color -> IO ()
setTextureColor (Texture t _) color = SDL.textureColorMod t $= color

spriteSize :: V2 CInt
spriteSize = V2 (fromIntegral tileSize) (fromIntegral tileSize)

renderTile :: SDL.Renderer -> Texture -> Position -> Tile -> IO ()
renderTile r t pos (Tile glyph fgColor bgColor) = do
    let point = P $ fmap (*tileSize) $ fmap fromIntegral pos
    let renderGlyph g = renderTexture r t point $ Just $ SDL.Rectangle (P (fmap (*tileSize) g)) spriteSize
    setTextureColor t bgColor
    renderGlyph filledGlyph
    setTextureColor t fgColor
    renderGlyph glyph

tileSize, textureWidth, textureHeight :: CInt
tileSize = 12
textureWidth = mapWidth * tileSize
textureHeight = mapHeight * tileSize

renderTexture :: SDL.Renderer -> Texture -> Point V2 CInt -> Maybe (SDL.Rectangle CInt) -> IO ()
renderTexture r (Texture t size) xy clip =
    let dstSize = maybe size (\(SDL.Rectangle _ size') -> size') clip
    in SDL.copy r t clip (Just (SDL.Rectangle xy dstSize))

setTextureBlendMode :: Texture -> SDL.BlendMode -> IO ()
setTextureBlendMode (Texture t _) bm = SDL.textureBlendMode t $= bm

play
  :: w
  -> System w TileImage -- ^ Drawing function
  -> (SDL.EventPayload -> System w ()) -- ^ Event handling function
  -> (Float  -> System w Bool) -- ^ Stepping function, with a time delta argument.
  -> IO ()
play initialWorld draw handle step = do
    
    -- init and show window
    SDL.initialize [SDL.InitVideo]
    SDL.HintRenderScaleQuality $= SDL.ScaleLinear
    renderQuality <- SDL.get SDL.HintRenderScaleQuality
    when (renderQuality /= SDL.ScaleLinear) $  putStrLn "Warning: Linear texture filtering not enabled!"
    window <- SDL.createWindow "mallRL" SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight}
    SDL.showWindow window

    -- init and show renderer
    let rendererConfig = SDL.RendererConfig { SDL.rendererType = SDL.AcceleratedVSyncRenderer, SDL.rendererTargetTexture = False}
    renderer <- SDL.createRenderer window (-1) rendererConfig

    targetTexture <- createBlank renderer (V2 textureWidth textureHeight) SDL.TextureAccessTarget
    spriteSheetTexture <- loadTexture renderer "resources/font_custom.bmp"

    let loop world previousImage = do

            -- handle events
            events <- SDL.pollEvents
            let eventPayloads = map SDL.eventPayload events
            let quit = elem SDL.QuitEvent $ eventPayloads
            let handle' w evt = runWith w $ handle evt >> ask
            worldAfterEvents <- foldM handle' world eventPayloads

            -- step world
            let t = 0.1
            (worldAfterStepping, rerenderNecessary) <- runWith worldAfterEvents $ do
                rerenderNecessary <- step t
                worldAfterStepping <- ask
                return (worldAfterStepping, rerenderNecessary)

            nextImage <- if rerenderNecessary
                then do
                    -- render map to texture
                    tileImage <- runWith worldAfterStepping draw
                    setAsRenderTarget renderer (Just targetTexture)
                    renderTileMap renderer spriteSheetTexture previousImage tileImage
                    return tileImage
                else return previousImage

            -- render texture to screen
            setAsRenderTarget renderer Nothing
            SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
            SDL.clear renderer
            let renderPosition = P (V2 0 0)
            renderTexture renderer targetTexture renderPosition Nothing
            SDL.present renderer

            -- sleep
            Control.Concurrent.threadDelay  16000 -- 33333

            --repeat
            unless quit $ loop worldAfterStepping nextImage

    loop initialWorld emptyMap

    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit

emptyMap :: TileImage
emptyMap = TileImage $ Array.listArray arrayBounds $ cycle [tileEmpty]