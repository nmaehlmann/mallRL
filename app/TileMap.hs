module TileMap where
import TileImage
import Colors
import Foreign.C.Types
import Position
import qualified Data.Array as Array
import Linear

mapWidth, mapHeight :: CInt
mapWidth = 100
mapHeight = 100

arrayBounds :: (Position, Position)
arrayBounds = (V2 0 0, fmap fromIntegral (V2 mapWidth mapHeight))

testMap :: TileImage
testMap = TileImage $ Array.listArray arrayBounds $ cycle [ground1, ground2]

emptyMap :: TileImage
emptyMap = TileImage $ Array.listArray arrayBounds $ cycle [tileEmpty]