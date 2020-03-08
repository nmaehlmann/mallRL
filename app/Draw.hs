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
import Data.Array

cDrawOutdoors :: DrawingFunction -> Position -> Room -> [(Position, Tile)] -> (CPosition, CDrawable) -> [(Position, Tile)]
cDrawOutdoors drawFun playerPosition mallRoom tm drawInfo@(CPosition pos, _) =
    if containsPosition pos (shrink1 mallRoom) 
        then cDrawWithCam drawFun playerPosition tm (CPosition pos, dMallRoof)
        else cDrawWithCam drawFun playerPosition tm drawInfo

cDrawIndoors :: DrawingFunction -> Position -> [Room] -> [(Position, Tile)] -> (CPosition, CDrawable) -> [(Position, Tile)]
cDrawIndoors drawFun playerPosition roomsToDraw tm drawInfo@(CPosition pos, _)  = 
    if elem True $ map (containsPosition pos) roomsToDraw 
        then cDrawWithCam drawFun playerPosition tm drawInfo
        else tm

cDrawWithCam :: DrawingFunction -> Position -> [(Position, Tile)] -> (CPosition, CDrawable) -> [(Position, Tile)]
cDrawWithCam drawFun (V2 playerX playerY) tm (CPosition pos, drawable) = drawDrawable drawFun tm (pos + camera, drawable)
    where
        camera = V2 xOff yOff
        xOff = (div (mapWidthInt - sidebarSize) 2) - playerX
        yOff = (div (mapHeightInt - logSize) 2) - playerY

draw :: System' TileImage
draw = do
    (CMallRoom mallRoom) <- get global
    let drawLayer drawFun tileImage = flip cfoldM emptyMap $ \tm (CPlayer, CPosition playerPosition, CIsInRoom roomsToDraw) -> do
            let cDraw = if containsPosition playerPosition mallRoom 
                then cDrawIndoors drawFun playerPosition roomsToDraw 
                else cDrawOutdoors drawFun playerPosition mallRoom
            newAssocs <- cfold cDraw []
            let (TileImage emptyArr) = tileImage
            (return (TileImage (emptyArr // (reverse newAssocs)))) :: System' TileImage
    bg <- drawLayer drawBG emptyMap
    drawLayer (drawFG bg) bg >>= drawUI