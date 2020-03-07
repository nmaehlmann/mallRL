module Car where
import Control.Monad.Random
import Apecs hiding (Map, Set)
import Linear
import Colors
import World
import CDrawable

flipV (DrawableBG g c1 c2) = DrawableBG g c2 c1
flipV a = a

windowColor = V3 102 178 255

carTop cMain cSide = Drawable (V2 12 13) cMain
carTireLeftTop cMain cSide = DrawableBG (V2 13 13) black cSide
carTireRightTop cMain cSide = flipV $ carTireLeftTop cMain cSide
carRoof cMain cSide = DrawableBG (V2 0 15) black cSide
carSideLeft cMain cSide = DrawableBG (V2 13 13) cMain cSide
carSideRight cMain cSide = flipV $ carSideLeft cMain cSide
carWindowLeft cMain cSide = DrawableBG (V2 13 13) cMain windowColor
carWindowRight cMain cSide = flipV $ carWindowLeft cMain cSide
carWindowMiddle cMain cSide = DrawableBG (V2 1 11) white windowColor
carTireLeftBot cMain cSide = DrawableBG (V2 13 13) black cMain
carTireRightBot cMain cSide = flipV $ carTireLeftBot cMain cSide
carFrontLeft cMain cSide = DrawableBG (V2 10 13) black cMain
carFrontRight cMain cSide = DrawableBG (V2 15 11) black cMain
carFrontMiddle cMain cSide = DrawableBG (V2 7 13) black cMain
carMiddle cMain cSide = DrawableBG (V2 10 11) black cMain

car = 
    [ [carTop, carTop, carTop]
    , [carTireLeftTop, carRoof, carTireRightTop]
    , [carSideLeft, carRoof, carSideRight]
    , [carSideLeft, carRoof, carSideRight]
    , [carWindowLeft, carWindowMiddle, carWindowRight]
    , [carTireLeftBot, carMiddle, carTireRightBot]
    , [carFrontLeft, carFrontMiddle, carFrontRight]
    ]

carWidth = 3
carHeight = 7

mkParkingLot :: Int -> Int -> Int -> System' ()
mkParkingLot xOff yOff cols = do
    flip mapM_ [0..(cols-1)] $ \col -> do
        let xOffC = xOff + col * (carWidth + 3)
        mkCar xOffC yOff
        mkCarLineH (xOffC - 1) (yOff + carHeight)
        mkCar xOffC (yOff + carHeight + 1)
        when (col /= (cols - 1)) $ do 
            newEntity (CPosition (V2 ((carWidth + 1) + xOffC) (yOff + carHeight)), dCross)
            mkCarLineV ((carWidth + 1) + xOffC) yOff
            mkCarLineV ((carWidth + 1) + xOffC) (yOff + carHeight + 1)

mkCarLineV :: Int -> Int -> System' ()
mkCarLineV xOff yOff = do
    flip mapM_ [(V2 xOff (y + yOff)) | y <- [0 .. carHeight - 1]] $ \p -> do
        newEntity (CPosition p, dLineV)

mkCarLineH :: Int -> Int -> System' ()
mkCarLineH xOff yOff = do
    flip mapM_ [(V2 (x + xOff) yOff) | x <- [0 .. carWidth + 1]] $ \p -> do
        newEntity (CPosition p, dLineH)

mkCar :: Int -> Int -> System' ()
mkCar xOff yOff = do
    flip mapM_ [(V2 x y) | x <- [0..2], y <- [0..6]] $ \p@(V2 x y) -> do
        newEntity (CPosition (p + (V2 xOff yOff)), (car !! y !! x) (V3 0 51 102) (V3 0 89 178))
