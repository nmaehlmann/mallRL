module CDrawable where
import TileImage
import Apecs

data CDrawable = Drawable Glyph Color | DrawableBG Glyph Color Color