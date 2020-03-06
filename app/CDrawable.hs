module CDrawable where
import TileImage
import Position
import Data.Array
import Linear
import TileMap

data CDrawable = Drawable Glyph Color | DrawableBG Glyph Color Color

drawDrawable :: TileImage -> (Position, CDrawable) -> TileImage
drawDrawable tm all@((V2 x y), d) = if x <= xMax && y <= yMax && x >= 0 && y >= 0 then drawDrawableUnsafe tm all else tm

drawDrawableUnsafe :: TileImage -> (Position, CDrawable) -> TileImage
drawDrawableUnsafe (TileImage tm) ((pos, Drawable glyph color)) = 
    let (Tile _ _ bgColor) = tm ! pos
    in TileImage $ tm // [(pos, Tile glyph color bgColor)]
drawDrawableUnsafe (TileImage tm) ((pos, DrawableBG glyph color bgColor)) = TileImage $ tm // [(pos, Tile glyph color bgColor)]