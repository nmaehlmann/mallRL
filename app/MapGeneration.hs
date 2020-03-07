module MapGeneration where
import Control.Monad.Random
import Apecs hiding (Map, Set)
import Linear
import Position
import Colors
import Item
import World
import Data.List
import Room
import Car
import RandomUtility

data ShelfType = ShelfHor | ShelfVer | ShelfSquare

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

entracePositions :: RandomGen g => [Room] -> Rand g [Position]
entracePositions rooms = do
    outerLeftRoom <- pickRandom $ filter (isOuterLeftRoom rooms) rooms
    pickDoor $ leftWall outerLeftRoom

isOuterLeftRoom :: [Room] -> Room -> Bool
isOuterLeftRoom rs rLeft = null $ concatMap (\rRight -> intersect (leftWall rLeft) (rightWall rRight)) rs

isOuterRightRoom :: [Room] -> Room -> Bool
isOuterRightRoom rs r = null $ concatMap (\r -> intersect (rightWall r) (leftWall r)) rs

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

getRoomsForPosition :: [Room] -> Position -> [Room]
getRoomsForPosition rs p = filter (containsPosition p) rs

initializeMap :: System' [CCar]
initializeMap = do
    cars1 <- mkParkingLot 10 5 5
    cars2 <- mkParkingLot 10 25 5
    cars3 <- mkParkingLot 10 45 5
    (rooms, walls) <- lift $ evalRandIO $ createMall 40 3
    flip mapM_ (allGroundPositions rooms) $ \p -> do
        newEntity (CPosition p, dGround, CIsInRoom (getRoomsForPosition rooms p))
    ds <- evalRandom $ do
        doors <- doorPositions rooms
        entrance <- entracePositions rooms
        return $ doors ++ entrance
    mapM_ (initializeWall ds) walls
    flip mapM_ rooms $ \r -> do
        shelfType <- evalRandom $ pickRandom [ShelfVer, ShelfHor]
        fillRoom shelfType r
    return $ cars1 ++ cars2 ++ cars3

initializeWall :: [Position] -> Wall -> System' ()
initializeWall doors (Wall x y x2 y2) = do
    let positions = filter (\p -> not (elem p doors)) [(V2 wx wy) | wx <- [x .. x2], wy <- [y .. y2]]
    flip mapM_ positions $ \p -> do
        newEntity (CSolid, CPosition p, dWall)
    return ()

createMall :: RandomGen g => Int -> Int ->  Rand g ([Room], [Wall])
createMall x y = do
    initialW <- getRandomR (30,50)
    initialH <- getRandomR (30,50)
    let wW = initialW + 1
    let wH = initialH + 1
    let wTop = Wall x y (x + wW) y
    let wBot = Wall x (y + wH) (x + wW) (y + wH)
    let wLeft = Wall x y x (y + wH)
    let wRight = Wall (x + wW) y (x + wW) (y + wH)
    let initialRoom = Room (x+1) (y+1) initialW initialH
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

type ItemChooser = Direction -> Rand StdGen Item

alwaysNacho :: ItemChooser
alwaysNacho _ = return Nachos

dirItemChooser :: Item -> Item -> Item -> Item  -> ItemChooser
dirItemChooser l _ _ _ DirLeft = return l
dirItemChooser _ r _ _ DirRight = return r
dirItemChooser _ _ u _ DirUp = return u
dirItemChooser _ _ _ d DirDown = return d

randomItemChooser :: ItemChooser
randomItemChooser _ = pickRandom allItems

mkShelfVer :: Position -> ItemChooser -> System' ()
mkShelfVer (V2 x' y) itemChooser = do
    let x = x' + 1
    let l = shelfTypeHeight ShelfVer
    flip mapM_ [V2 x ys | ys <- [y + 1 .. y + l - 2] ] $ \p -> do
        -- shelf middle
        newEntity (CSolid, CPosition p, dWall)
        
        -- shelf left
        leftItem <- lift $ evalRandIO (itemChooser DirLeft)
        newEntity (CSolid, CPosition (left p), dShelf)
        newEntity (CItem leftItem, CPosition (left p), lookupItemDrawable leftItem)
        
        -- shelf right
        rightItem <- lift $ evalRandIO (itemChooser DirRight)
        newEntity (CSolid, CPosition (right p), dShelf)
        newEntity (CItem rightItem, CPosition (right p), lookupItemDrawable rightItem)

    flip mapM_ [V2 xs y | xs <- [x - 1 .. x + 1] ] $ \p -> do
        -- shelf north bound
        newEntity (CSolid, CPosition p, dShelfNorth)

    flip mapM_ [V2 xs (y + l - 1) | xs <- [x - 1 .. x + 1] ] $ \p -> do
        -- shelf south bound
        newEntity (CSolid, CPosition p, dShelfSouth)

mkShelfHor :: Position -> ItemChooser -> System' ()
mkShelfHor (V2 x y') itemChooser = do
    let y = y' + 1
    let l = shelfTypeWidth ShelfHor
    flip mapM_ [V2 xs y | xs <- [x + 1 .. x + l - 2] ] $ \p -> do
        -- shelf middle
        newEntity (CSolid, CPosition p, dWall)
        
        -- shelf up
        leftItem <- lift $ evalRandIO (itemChooser DirLeft)
        newEntity (CSolid, CPosition (up p), dShelf)
        newEntity (CItem leftItem, CPosition (up p), lookupItemDrawable leftItem)
        
        -- shelf down
        rightItem <- lift $ evalRandIO (itemChooser DirRight)
        newEntity (CSolid, CPosition (down p), dShelf)
        newEntity (CItem rightItem, CPosition (down p), lookupItemDrawable rightItem)

    flip mapM_ [V2 x ys | ys <- [y - 1 .. y + 1] ] $ \p -> do
        -- shelf west bound
        newEntity (CSolid, CPosition p, dShelfWest)

    flip mapM_ [V2 (x + l - 1) ys | ys <- [y - 1 .. y + 1] ] $ \p -> do
        -- shelf east bound
        newEntity (CSolid, CPosition p, dShelfEast)


pickItemChooser :: RandomGen g => Rand g ItemChooser
pickItemChooser = do
        l <- pickRandom allItems
        r <- pickRandom allItems
        u <- pickRandom allItems
        d <- pickRandom allItems
        pickRandom [dirItemChooser l r u d, randomItemChooser]

fillRoom :: ShelfType -> Room -> System' ()
fillRoom st (Room x y w h) = do
    let sp = shelfTypePadding st
    let sw = shelfTypeWidth st
    let sh = shelfTypeHeight st
    let horNumShelves = (w - sp) `div` (sp + sw)
    let xOff = (w - (sp + horNumShelves * (sp + sw))) `div` 2
    let verNumShelves = (h - sp) `div` (sp + sh)
    let yOff = (h - (sp + verNumShelves * (sp + sh))) `div` 2
    let shelfPositions = [(V2 (x + xOff + sp + ix * (sw + sp)) (y + yOff + sp + iy * (sh + sp))) | ix <- [0..horNumShelves - 1], iy <- [0..verNumShelves - 1]]
    flip mapM_ shelfPositions $ \p -> do
        chooser <- evalRandom pickItemChooser
        shelfGen st p chooser

shelfTypeWidth, shelfTypeHeight, shelfTypePadding :: ShelfType -> Int
shelfTypeWidth ShelfVer = 3
shelfTypeWidth ShelfHor = 7
shelfTypeHeight ShelfVer = 7
shelfTypeHeight ShelfHor = 3
shelfTypePadding ShelfVer = 2
shelfTypePadding ShelfHor = 2

shelfGen ShelfVer = mkShelfVer
shelfGen ShelfHor = mkShelfHor
