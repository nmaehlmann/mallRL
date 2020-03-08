module TileMap where
import Foreign.C.Types
import Position
import Linear

mapWidth, mapHeight :: CInt
mapWidth = 72  
mapHeight = 48

arrayBounds :: (Position, Position)
arrayBounds = (V2 0 0, fmap fromIntegral (V2 mapWidth mapHeight))

mapWidthInt, mapHeightInt :: Int
mapWidthInt = fromIntegral mapWidth
mapHeightInt = fromIntegral mapHeight

xMax, yMax :: Int
xMax = fromIntegral (mapWidth - 1)
yMax = fromIntegral (mapHeight - 1)