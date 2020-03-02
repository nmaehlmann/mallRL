module Item where
import TileImage
import Colors
import CDrawable
import Linear

data Item = Pizza

dPizza = DrawableBG pizzaGlyph pizzaFG pizzaBG
pizzaGlyph = V2 15 4
pizzaFG = V3 222 222 222
pizzaBG = V3 217 0 0