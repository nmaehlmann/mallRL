module Item where
import Colors
import CDrawable
import Linear

data Item = Pizza | Seaweed

dPizza = DrawableBG pizzaGlyph pizzaFG pizzaBG
pizzaGlyph = V2 15 4
pizzaFG = V3 222 222 222
pizzaBG = V3 217 0 0

dSeaweed = DrawableBG seaweedGlyph seaweedFG seaweedBG
seaweedGlyph = V2 7 15
seaweedFG = V3 0 102 2
seaweedBG = black