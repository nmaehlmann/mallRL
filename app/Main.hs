{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Word
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import Control.Concurrent
import Data.Array (Array)
import qualified Data.Array as Array
import Text.Printf
import System.CPUTime
import System.IO

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

data Texture = Texture SDL.Texture (V2 CInt)

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
  let key = V4 0 maxBound maxBound maxBound
  SDL.surfaceColorKey surface $= Just key
  t <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  return (Texture t size)

type Position = (Int, Int)
type Color = V3 Word8
data Tile = Tile (V2 CInt) Color Color
data TileImage = TileImage (Array Position Tile)

renderTileMap :: SDL.Renderer -> Texture -> TileImage -> IO ()
renderTileMap r t (TileImage arr) = mapM st (Array.assocs arr) >> return ()
  where st (pos, tile) = renderTile r t pos tile

setTextureColor :: Texture -> Color -> IO ()
setTextureColor (Texture t _) color = SDL.textureColorMod t $= color

renderTile :: SDL.Renderer -> Texture -> Position -> Tile -> IO ()
renderTile r t (x, y) (Tile tileNo fgColor bgColor) = do
  let spriteSize = V2 12 12
  let point = P $ fmap (*12) $ V2 (fromIntegral x) (fromIntegral y)
  setTextureColor t bgColor
  renderTexture r t point $ Just $ SDL.Rectangle (P (fmap (*12) (V2 11 13))) spriteSize
  setTextureColor t fgColor
  renderTexture r t point $ Just $ SDL.Rectangle (P (fmap (*12) tileNo)) spriteSize

testMap :: TileImage
testMap = TileImage $ Array.listArray ((0,0), (100,50)) $ cycle [tile1,tile2,tile1,tile2,tile1,tile2]

tile1 :: Tile
tile1 = Tile (V2 4 0) (V3 200 100 100) (V3 230 230 230)

tile2 :: Tile
tile2 = Tile (V2 5 1) (V3 200 100 200) (V3 130 30 30)

renderTexture :: SDL.Renderer -> Texture -> Point V2 CInt -> Maybe (SDL.Rectangle CInt) -> IO ()
renderTexture r (Texture t size) xy clip =
  let dstSize = maybe size (\(SDL.Rectangle _ size') -> size') clip
  in SDL.copy r t clip (Just (SDL.Rectangle xy dstSize))

setTextureAlpha :: Texture -> Word8 -> IO ()
setTextureAlpha (Texture t _) rgb = SDL.textureAlphaMod t $= rgb

setTextureBlendMode :: Texture -> SDL.BlendMode -> IO ()
setTextureBlendMode (Texture t _) bm = SDL.textureBlendMode t $= bm

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "SDL Tutorial"
      SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight}
  SDL.showWindow window

  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
        { SDL.rendererType = SDL.AcceleratedVSyncRenderer
        , SDL.rendererTargetTexture = False
        }

  spriteSheetTexture <- loadTexture renderer "font.bmp"
  setTextureBlendMode spriteSheetTexture SDL.BlendAlphaBlend

  let loop = do
        events <- SDL.pollEvents
        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
        SDL.rendererDrawColor renderer $= V4 30 30 30 255
        SDL.clear renderer
        time $ renderTileMap renderer spriteSheetTexture testMap
        SDL.present renderer
        -- Control.Concurrent.threadDelay 16666
        unless quit loop

  loop

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit