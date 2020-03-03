module Item where
import Colors
import CDrawable
import Linear

data Item = Pizza | Seaweed | Bananas | Fishsticks
    deriving (Eq, Show)

lookupItemDrawable :: Item -> CDrawable
lookupItemDrawable Pizza = dPizza
lookupItemDrawable Seaweed = dSeaweed
lookupItemDrawable Bananas = dBananas
lookupItemDrawable Fishsticks = dFishsticks

dPizza = DrawableBG pizzaGlyph pizzaFG pizzaBG
pizzaGlyph = V2 15 4
pizzaFG = V3 222 222 222
pizzaBG = V3 217 0 0

dSeaweed = DrawableBG seaweedGlyph seaweedFG seaweedBG
seaweedGlyph = V2 7 15
seaweedFG = V3 0 102 2
seaweedBG = black

dBananas = DrawableBG bananasGlyph bananasFG bananasBG
bananasGlyph = V2 15 10
bananasFG = V3 255 255 102
bananasBG = black

dFishsticks = DrawableBG fishsticksGlyph fishsticksFG fishsticksBG
fishsticksGlyph = V2 13 3
fishsticksFG = V3 255 178 102
fishsticksBG = V3 51 153 255