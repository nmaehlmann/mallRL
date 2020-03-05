module TileMap where
import TileImage
import Colors
import Foreign.C.Types
import Position
import qualified Data.Array as Array
import Linear

mapWidth, mapHeight :: CInt
mapWidth = 60
mapHeight = 30

arrayBounds :: (Position, Position)
arrayBounds = (V2 0 0, fmap fromIntegral (V2 mapWidth mapHeight))

testMap :: TileImage
testMap = TileImage $ Array.listArray arrayBounds $ cycle [ground1, ground2]

emptyMap :: TileImage
emptyMap = TileImage $ Array.listArray arrayBounds $ cycle [tileEmpty]

mapWidthInt, mapHeightInt :: Int
mapWidthInt = fromIntegral mapWidth
mapHeightInt = fromIntegral mapHeight

xMax, yMax :: Int
xMax = fromIntegral (mapWidth - 1)
yMax = fromIntegral (mapHeight - 1)