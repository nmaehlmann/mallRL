{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Renderer where

import Control.Monad
import Data.Word
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import Control.Concurrent
import Data.Array (Array, (!))
import qualified Data.Array as Array
import Text.Printf
import System.CPUTime
import System.IO
import TileImage
import Position
import Apecs hiding (($=))

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 360)

data Texture = Texture SDL.Texture (V2 CInt)

createBlank :: SDL.Renderer -> V2 CInt -> SDL.TextureAccess -> IO Texture
createBlank r size access = Texture <$> SDL.createTexture r SDL.RGBA8888 access size <*> pure size

setAsRenderTarget :: SDL.Renderer -> Maybe Texture -> IO ()
setAsRenderTarget r Nothing = SDL.rendererRenderTarget r $= Nothing
setAsRenderTarget r (Just (Texture t _)) = SDL.rendererRenderTarget r $= Just t

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

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
            unless (tileOld == tileNew) $ renderTile r t pos tileNew
    
setTextureColor :: Texture -> Color -> IO ()
setTextureColor (Texture t _) color = SDL.textureColorMod t $= color

spriteSize :: V2 CInt
spriteSize = V2 (fromIntegral tileSize) (fromIntegral tileSize)

filled :: Glyph
filled = (V2 11 13)

renderTile :: SDL.Renderer -> Texture -> Position -> Tile -> IO ()
renderTile r t pos (Tile glyph fgColor bgColor) = do
    let point = P $ fmap (*tileSize) $ fmap fromIntegral pos
    let renderGlyph g = renderTexture r t point $ Just $ SDL.Rectangle (P (fmap (*tileSize) g)) spriteSize
    setTextureColor t bgColor
    renderGlyph filled
    setTextureColor t fgColor
    renderGlyph glyph

mapWidth, mapHeight, textureWidth, textureHeight :: CInt
mapWidth = 100
mapHeight = 100
tileSize = 12
textureWidth = mapWidth * tileSize
textureHeight = mapHeight * tileSize

arrayBounds :: (Position, Position)
arrayBounds = (V2 0 0, fmap fromIntegral (V2 mapWidth mapHeight))

testMap :: TileImage
testMap = TileImage $ Array.listArray arrayBounds $ cycle [tile1,tile2]

emptyMap :: TileImage
emptyMap = TileImage $ Array.listArray arrayBounds $ cycle [tileEmpty]

tile1 :: Tile
tile1 = Tile (V2 4 0) (V3 150 100 100) (V3 255 150 255)

tile2 :: Tile
tile2 = Tile (V2 14 0) (V3 150 100 100) (V3 255 255 150)

tileEmpty :: Tile
tileEmpty = Tile filled (V3 0 0 0) (V3 0 0 0)

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
  -> (Float  -> System w ()) -- ^ Stepping function, with a time delta argument.
  -> IO ()
play initialWorld draw handle step = do
    
    -- init and show window
    SDL.initialize [SDL.InitVideo]
    SDL.HintRenderScaleQuality $= SDL.ScaleLinear
    renderQuality <- SDL.get SDL.HintRenderScaleQuality
    when (renderQuality /= SDL.ScaleLinear) $  putStrLn "Warning: Linear texture filtering not enabled!"
    window <- SDL.createWindow "Mall Game" SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight}
    SDL.showWindow window

    -- init and show renderer
    let rendererConfig = SDL.RendererConfig { SDL.rendererType = SDL.AcceleratedVSyncRenderer, SDL.rendererTargetTexture = False}
    renderer <- SDL.createRenderer window (-1) rendererConfig

    targetTexture <- createBlank renderer (V2 textureWidth textureHeight) SDL.TextureAccessTarget
    spriteSheetTexture <- loadTexture renderer "font.bmp"

    let loop world previousImage = do

            -- handle events
            events <- SDL.pollEvents
            let eventPayloads = map SDL.eventPayload events
            let quit = elem SDL.QuitEvent $ eventPayloads
            let handle' w evt = runWith w $ handle evt >> ask
            worldAfterEvents <- foldM handle' world eventPayloads

            -- step world
            let t = 0.1
            worldAfterStepping <- runWith worldAfterEvents $ step t >> ask

            -- render map to texture
            tileImage <- runWith worldAfterStepping draw
            setAsRenderTarget renderer (Just targetTexture)
            time $ renderTileMap renderer spriteSheetTexture previousImage tileImage

            -- render texture to screen
            setAsRenderTarget renderer Nothing
            SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
            SDL.clear renderer
            let renderPosition = P (V2 0 0)
            renderTexture renderer targetTexture renderPosition Nothing
            SDL.present renderer

            -- sleep
            -- Control.Concurrent.threadDelay 16666

            --repeat
            unless quit $ loop worldAfterStepping tileImage

    loop initialWorld emptyMap

    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit

