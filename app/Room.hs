module Room where
import Position
import Linear
import Data.List

data Room = Room Int Int Int Int deriving Eq
data Wall = Wall Int Int Int Int

shrink1 :: Room -> Room
shrink1 (Room x y w h) = Room (x+1) (y+1) (w-2) (h-2)

containsPosition :: Position -> Room -> Bool
containsPosition (V2 px py) (Room x y w h) = px >= x - 1 && px <= x + w && py >= y - 1 && py <= y + h

groundPositions :: Room -> [Position]
groundPositions (Room x y w h)  = [(V2 xs ys) | xs <- [x - 1 .. x + w], ys <- [y - 1 .. y + h]]

allGroundPositions :: [Room] -> [Position]
allGroundPositions rs = nub $ concatMap groundPositions rs