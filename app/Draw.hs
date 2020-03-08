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
import Colors

draw :: System' TileImage
draw = flip cfoldM emptyMap $ \tm (CPlayer, CPosition playerPosition, CIsInRoom roomsToDraw) -> do
    (CMallRoom mallRoom) <- get global
    let cDraw = if containsPosition playerPosition mallRoom then cDrawIndoors playerPosition roomsToDraw else cDrawOutdoors playerPosition mallRoom
    cfold cDraw emptyMap >>= drawUI

cDrawOutdoors :: Position -> Room -> TileImage -> (CPosition, CDrawable) -> TileImage
cDrawOutdoors playerPosition mallRoom tm drawInfo@(CPosition pos, _) =
    if containsPosition pos (shrink1 mallRoom) 
        then cDrawWithCam playerPosition tm (CPosition pos, dMallRoof)
        else cDrawWithCam playerPosition tm drawInfo

cDrawIndoors :: Position -> [Room] -> TileImage -> (CPosition, CDrawable) -> TileImage
cDrawIndoors playerPosition roomsToDraw tm drawInfo@(CPosition pos, _)  = 
    if elem True $ map (containsPosition pos) roomsToDraw 
        then cDrawWithCam playerPosition tm drawInfo
        else tm

cDrawWithCam :: Position -> TileImage -> (CPosition, CDrawable) -> TileImage
cDrawWithCam (V2 playerX playerY) tm (CPosition pos, drawable) = drawDrawable tm (pos + camera, drawable)
    where
        camera = V2 xOff yOff
        xOff = (div (mapWidthInt - sidebarSize) 2) - playerX
        yOff = (div (mapHeightInt - logSize) 2) - playerY