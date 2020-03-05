module CDrawable where
import TileImage
import Position
import Data.Array

data CDrawable = Drawable Glyph Color | DrawableBG Glyph Color Color

drawDrawable :: TileImage -> (Position, CDrawable) -> TileImage
drawDrawable (TileImage tm) ((pos, Drawable glyph color)) = 
    let (Tile _ _ bgColor) = tm ! pos
    in TileImage $ tm // [(pos, Tile glyph color bgColor)]
drawDrawable (TileImage tm) ((pos, DrawableBG glyph color bgColor)) = TileImage $ tm // [(pos, Tile glyph color bgColor)]