module Room where
import Position
import Linear
import Data.List

data Room = Room Int Int Int Int deriving Eq
data Wall = Wall Int Int Int Int

containsPosition :: Position -> Room -> Bool
containsPosition p r = elem p $ groundPositions r

groundPositions :: Room -> [Position]
groundPositions (Room x y w h)  = [(V2 xs ys) | xs <- [x - 1 .. x + w], ys <- [y - 1 .. y + h]]

allGroundPositions :: [Room] -> [Position]
allGroundPositions rs = nub $ concatMap groundPositions rs