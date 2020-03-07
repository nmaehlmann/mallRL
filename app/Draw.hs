module Draw where
import Apecs hiding (Map, Set)
import Linear
import TileImage
import Position
import Renderer
import TileMap
import CDrawable
import World
import UI
import Room

draw :: System' TileImage
draw = flip cfoldM emptyMap $ \tm (CPlayer, CPosition playerPosition, CIsInRoom roomsToDraw) -> do
    cfold (cDrawDrawable playerPosition roomsToDraw) testMap >>= drawUI

cDrawDrawable :: Position -> [Room] -> TileImage -> (CPosition, CDrawable) -> TileImage
cDrawDrawable (V2 playerX playerY) roomsToDraw tm (CPosition pos, drawable) = 
    if True -- elem True $ map (containsPosition pos) roomsToDraw 
        then drawDrawable tm (pos + camera, drawable)
        else tm
    where
        camera = V2 xOff yOff
        xOff = (div (mapWidthInt - sidebarSize) 2) - playerX
        yOff = (div (mapHeightInt - logSize) 2) - playerY