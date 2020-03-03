module TileImage where
import Data.Array
import SDL.Vect
import Foreign.C.Types
import Data.Word
import Position

type Glyph = (V2 CInt)
type Color = V3 Word8
data Tile = Tile Glyph Color Color
    deriving (Show, Eq)
data TileImage = TileImage (Array Position Tile)
    deriving (Eq)