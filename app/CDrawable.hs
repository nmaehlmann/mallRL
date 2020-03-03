module CDrawable where
import TileImage

data CDrawable = Drawable Glyph Color | DrawableBG Glyph Color Color