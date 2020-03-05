module TerminalText where
import TileImage
import CDrawable
import Position
import Colors
import Linear

data TerminalText = FGText String Color | BGText String Color Color | ConcatText TerminalText TerminalText | Icon CDrawable
instance Semigroup TerminalText where
    t1 <> t2 = ConcatText t1 t2

textLen :: TerminalText -> Int
textLen (FGText s _) = length s
textLen (BGText s _ _) = length s
textLen (ConcatText t1 t2) = textLen t1 + textLen t2
textLen (Icon _) = 1

textToDrawables :: TerminalText -> [CDrawable]
textToDrawables (FGText s c) = map (\g -> Drawable g c) $ map charToGlyph s
textToDrawables (BGText s cFG cBG) = map (\g -> DrawableBG g cFG cBG) $ map charToGlyph s
textToDrawables (ConcatText t1 t2) = textToDrawables t1 ++ textToDrawables t2
textToDrawables (Icon d) = [d]

drawText :: Position -> TerminalText -> TileImage -> TileImage
drawText (V2 x y) txt tm = foldl drawDrawable tm $ zip [V2 xs y | xs <- [x..]] (textToDrawables txt)
