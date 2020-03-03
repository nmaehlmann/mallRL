module Colors where
import TileImage
import Linear
import CDrawable

tileEmpty = Tile filledGlyph black black

ground1 = Tile groundGlyph1 groundFGColor groundBGColor
ground2 = Tile groundGlyph1 groundFGColor groundBGColor
groundGlyph1 = V2 14 15
groundGlyph2 = V2 4 0
groundBGColor = V3 255 140 103
groundFGColor = V3 255 178 102

dPlayer = Drawable playerGlyph playerColor
playerGlyph = V2 0 4
playerColor = white

dWall = Drawable wallGlyph wallColor
wallGlyph = V2 11 13
wallColor = white

filledGlyph = V2 11 13

black = V3 0 0 0
white = V3 255 255 255

