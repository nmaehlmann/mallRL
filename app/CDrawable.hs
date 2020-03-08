module CDrawable where
import TileImage
import Position
import Data.Array
import Linear
import TileMap

data CDrawable = Drawable Glyph Color | DrawableBG Glyph Color Color

type DrawingFunction = [(Position, Tile)] -> (Position, CDrawable) -> [(Position, Tile)]

drawFG :: TileImage -> DrawingFunction
drawFG (TileImage tm) ls (pos, Drawable glyph color) = 
    let (Tile _ _ bgColor) = tm ! pos
    in (pos, Tile glyph color bgColor) : ls
drawFG _ ls _= ls

drawBG :: DrawingFunction
drawBG ls (pos, DrawableBG glyph color bgColor) = (pos, Tile glyph color bgColor) : ls
drawBG ls _ = ls

drawDrawable :: DrawingFunction -> [(Position, Tile)] -> (Position, CDrawable) -> [(Position, Tile)]
drawDrawable drawFun tm all@((V2 x y), d) = if x < xMax && y < yMax && x >= 0 && y >= 0 then drawFun tm all else tm