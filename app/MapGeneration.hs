module MapGeneration where
import Control.Monad.Random
import System.Random
import Apecs hiding (Map, Set)
import Linear
import TileImage
import Position
import Data.Array
import Colors
import TileMap
import CDrawable
import Item
import Apecs.Experimental.Reactive
import Control.Monad
import World
import Data.List

data Room = Room Int Int Int Int deriving Eq
data Wall = Wall Int Int Int Int

minRoomLen, minRoomSize :: Int
minRoomLen = 10
minRoomSize = 100

leftWall, rightWall, upperWall, lowerWall :: Room -> [Position]
leftWall (Room x y _ h) = [V2 (x - 1) ys | ys <- [y .. y + h - 1]]
rightWall (Room x y w h) = [V2 (x + w) ys | ys <- [y .. y + h - 1]]
upperWall (Room x y w _) = [V2 xs (y - 1) | xs <- [x .. x + w - 1]]
lowerWall (Room x y w h) = [V2 xs (y + h) | xs <- [x .. x + w - 1]]

doorPositions :: RandomGen g => [Room] -> Rand g [Position]
doorPositions rooms = do
    let doorWalls = filter (\l -> length l >= doorSize) [checkNeighbours r1 r2 | r1 <- rooms, r2 <- rooms]
    concat <$> mapM pickDoor doorWalls

checkNeighbours :: Room -> Room -> [Position]
checkNeighbours r1 r2 = 
    let l = intersect (leftWall r1) (rightWall r2)
        u = intersect (upperWall r1) (lowerWall r2)
    in  l ++ u

doorSize = 3

pickDoor :: RandomGen g => [Position] -> Rand g [Position]
pickDoor ps = do
    start <- getRandomR (0, length ps - 1 - doorSize)
    return $ take doorSize $ drop start ps


initializeMap :: System' ()
initializeMap = do
    (rooms, walls) <- lift $ evalRandIO createRooms
    doors <- lift $ evalRandIO $ doorPositions rooms
    mapM_ (initializeWall doors) walls
    return ()

initializeWall :: [Position] -> Wall -> System' ()
initializeWall doors (Wall x y x2 y2) = do
    let positions = filter (\p -> not (elem p doors)) [(V2 wx wy) | wx <- [x .. x2], wy <- [y .. y2]]
    flip mapM_ positions $ \p -> do
        newEntity (CSolid, CPosition p, dWall)
    return ()

createRooms :: RandomGen g => Rand g ([Room], [Wall])
createRooms = do
    initialW <- getRandomR (30,50)
    initialH <- getRandomR (30,50)
    let wW = initialW + 1
    let wH = initialH + 1
    let wTop = Wall 0 0 wW 0
    let wBot = Wall 0 wH wW wH
    let wLeft = Wall 0 0 0 wH
    let wRight = Wall wW 0 wW wH
    let initialRoom = Room 1 1 wW wH
    (rooms, walls) <- divideRoom initialRoom
    return (rooms, walls ++ [wTop, wBot, wLeft, wRight])

divideRoom :: RandomGen g => Room ->  Rand g ([Room], [Wall])
divideRoom room = do
    divRes <- divide room
    case divRes of
        Just (wall, room1, room2) -> do
            (room1s, wall1s) <- divideRoom room1
            (room2s, wall2s) <- divideRoom room2
            return (room1s ++ room2s, [wall] ++ wall1s ++ wall2s)
        Nothing -> return ([room],[])

divide :: RandomGen g => Room -> Rand g (Maybe (Wall, Room, Room))
divide (Room x y w h) = chanceForNothing 0 $ do
    divHor <- getRandomR (0, 1)
    variance <- getRandomR (-3, 3)
    let fw = fromIntegral w :: Float
    let fh = fromIntegral h :: Float
    if divHor >= (fw / (fw + fh))
        then do -- [|]
            let h1 = div h 2 + variance
            let h2 = h - h1 - 1
            let wall = Wall x (y + h1) (x + w - 1) (y + h1)
            return $ do
                room1 <- constructRoom x y w h1
                room2 <- constructRoom x (y + h1 + 1) w h2
                return (wall, room1, room2)
        else do -- [-]
            let w1 = div w 2 + variance
            let w2 = w - w1 - 1
            let wall = Wall (x + w1) y (x + w1) (y + h - 1)
            return $ do
                room1 <- constructRoom x y w1 h
                room2 <- constructRoom (x + w1 + 1) y w2 h
                return (wall, room1, room2)

constructRoom :: Int -> Int -> Int -> Int -> Maybe Room
constructRoom x y w h = if valid then Just (Room x y w h) else Nothing
    where valid = w * h >= minRoomSize && w >= minRoomLen && h >= minRoomLen
            

chanceForNothing :: RandomGen g => Float -> Rand g (Maybe a) -> Rand g (Maybe a)
chanceForNothing c r = do
    b <- chance c
    if b then return Nothing else r

chance :: RandomGen g => Float -> Rand g Bool
chance f = do
    r <- getRandomR (0,100)
    return $ r < f 