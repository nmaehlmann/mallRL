module Item where
import Colors
import CDrawable
import Linear

data Item = Pizza | Seaweed | Bananas | Fishsticks | Nachos | Apples| Donut | Chocolate | Oranges | Grapes | Lightbulb | Mustard | Ketchup
    deriving (Eq, Show)

allItems = [Pizza, Seaweed, Bananas, Fishsticks, Nachos, Apples, Donut, Chocolate, Oranges, Grapes, Lightbulb, Mustard, Ketchup]

lookupItemDrawable :: Item -> CDrawable
lookupItemDrawable Pizza = dPizza
lookupItemDrawable Seaweed = dSeaweed
lookupItemDrawable Bananas = dBananas
lookupItemDrawable Fishsticks = dFishsticks
lookupItemDrawable Nachos = dNachos
lookupItemDrawable Apples = dApples
lookupItemDrawable Donut = dDonut
lookupItemDrawable Chocolate = dChocoloate
lookupItemDrawable Oranges = dOranges
lookupItemDrawable Grapes = dGrapes
lookupItemDrawable Lightbulb = dLightbulb
lookupItemDrawable Mustard = dMustard
lookupItemDrawable Ketchup = dKetchup

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

dNachos = DrawableBG nachosGlyph nachosFG nachosBG
nachosGlyph = V2 15 1
nachosFG = V3 255 204 51
nachosBG = V3 0 0 102

dApples = DrawableBG applesGlyph applesFG applesBG
applesGlyph = V2 5 9
applesFG = V3 217 0 0
applesBG = V3 163 217 0

dDonut = DrawableBG donutGlyph donutFG donutBG
donutGlyph = V2 9 0
donutFG = V3 255 102 178
donutBG = V3 102 82 105

dChocoloate = DrawableBG chocoloateGlyph chocoloateFG chocoloateBG
chocoloateGlyph = V2 3 2
chocoloateFG = black
chocoloateBG = V3 137 72 72

dOranges = DrawableBG orangesGlyph orangesFG orangesBG
orangesGlyph = V2 10 0
orangesFG = V3 255 178 102
orangesBG = V3 51 153 255

dGrapes = DrawableBG grapesGlyph grapesFG grapesBG
grapesGlyph = V2 5 0
grapesFG = V3 255 102 255
grapesBG = V3 70 140 0

dLightbulb = DrawableBG lightbulbGlyph lightbulbFG lightbulbBG
lightbulbGlyph = V2 15 0
lightbulbFG = V3 255 255 51
lightbulbBG = V3 191 191 191

dMustard = DrawableBG mustardGlyph mustardFG mustardBG
mustardGlyph = V2 0 1
mustardFG = V3 255 255 51
mustardBG = V3 191 191 191

dKetchup = DrawableBG ketchupGlyph ketchupFG ketchupBG
ketchupGlyph = V2 0 1
ketchupFG = V3 255 0 0
ketchupBG = V3 191 191 191
