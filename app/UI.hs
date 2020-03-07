{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia                #-}
module UI where
import Apecs hiding (Map, Set)
import Linear
import TileImage
import Position
import Data.Array
import Colors
import TileMap
import CDrawable
import Item
import World
import TerminalText

logSize :: Int
logSize = 20

sidebarSize :: Int
sidebarSize = 17

drawLog :: TileImage -> System' TileImage
drawLog (TileImage arr) = do
    let bg = fill black $ rect (V2 0 (yMax - logSize + 1 - 2)) mapWidthInt (logSize + 2)
    let tm = TileImage $ arr // bg
    (CLog txts) <- get global
    let paddedTxts = take logSize $ leftPad logSize (FGText "" black) txts
    let logPositions = [(V2 1 (y - 1)) | y <- (decrease yMax)]
    return $ applyToPairs drawText tm $ zip logPositions paddedTxts

decrease :: Int -> [Int]
decrease i = i : (decrease (i - 1))

applyToPairs :: (a -> b -> c -> c) -> c -> [(a, b)] -> c
applyToPairs f c ps = foldl (\tm (a, b) -> f a b tm) c ps

drawSidebarBG :: TileImage -> System' TileImage
drawSidebarBG (TileImage arr) = return $ TileImage $ arr // bg
    where bg = fill black $ rect (V2 (xMax - sidebarSize + 1) 0) sidebarSize mapHeightInt

drawShoppingListBG :: TileImage -> System' TileImage
drawShoppingListBG (TileImage arr) = return $ TileImage $ arr // bg
    where bg = fill (V3 255 102 102) $ rect (V2 (xMax - sidebarSize + 1 + 1) 1) (sidebarSize - 2) (mapHeightInt - 2)

drawShoppingListHeader :: TileImage -> System' TileImage
drawShoppingListHeader tm = return $ drawText (V2 shoppingListContentsX shoppingListHeaderY) shoppingListHeader tm
    where 
        shoppingListHeader = FGText "Shopping List" black -- <> heart <> heart <> heart
        heart = Icon (Drawable heartGlyph black)

drawInventoryHeader :: TileImage -> System' TileImage
drawInventoryHeader tm = do
    flip cfoldM tm $ \tm (CPlayer, CInventory items, CShoppingList shoppingList) -> do
        let shoppingListLen = length shoppingList
        let shoppingCart = FGText "#" black
        let inventoryHeader = FGText "Inventory" black
        return $ drawText (V2 shoppingListContentsX (inventoryHeaderY shoppingListLen)) inventoryHeader tm

shoppingListHeaderY = 2
shoppingListContentsX = xMax - sidebarSize + 3
shoppingListContentsY = shoppingListHeaderY + 2
inventoryHeaderY shoppingListLen = shoppingListContentsY + shoppingListLen + 2
inventoryContentsY shoppingListLen = inventoryHeaderY shoppingListLen + 2

drawInventoryContent :: TileImage -> System' TileImage
drawInventoryContent tm = 
    flip cfoldM tm $ \tm (CPlayer, CInventory inventory, CShoppingList shoppingList) -> do
        let shoppingListLen = length shoppingList
        let inventoryY = inventoryContentsY shoppingListLen
        let shoppingListPositions = [(V2 shoppingListContentsX y) | y <- [inventoryY ..]]
        return $ applyToPairs (drawItem []) tm $ zip shoppingListPositions $ reverse inventory

drawShoppingListContent :: TileImage -> System' TileImage
drawShoppingListContent tm = 
    flip cfoldM tm $ \tm (CPlayer, CShoppingList shoppingList, CInventory inventory) -> do
        let inInventory = filter (\i -> elem i inventory) shoppingList
        let notInInventory = filter (\i -> not (elem i inventory)) shoppingList
        let shoppingListPositions = [(V2 shoppingListContentsX y) | y <- [shoppingListContentsY ..]]
        return $ applyToPairs (drawItem inventory) tm $ zip shoppingListPositions $ notInInventory ++ inInventory

rect :: Position -> Int -> Int -> [Position]
rect (V2 sx sy) w h = [(V2 x y) | x <- [sx..(sx + w - 1)], y <- [sy .. (sy + h - 1)]]

fill :: Color -> [Position] -> [(Position, Tile)]
fill c ps = map (\p -> (p, Tile filledGlyph c c)) ps

drawItem :: [Item] -> Position -> Item -> TileImage -> TileImage
drawItem inventory pos item = if elem item inventory 
    then drawItemInInventory pos item
    else drawItemNotInInventory pos item

drawItemNotInInventory :: Position -> Item -> TileImage -> TileImage
drawItemNotInInventory pos item = drawText pos txt
    where 
        txt = Icon itemDrawable <> toText (" " ++ itemTxt)
        itemTxt = rightPad (sidebarSize - 4 - 2) ' ' $ show item
        itemDrawable = lookupItemDrawable item
        toText t = case itemDrawable of
            (Drawable _ fg) -> FGText t fg
            (DrawableBG _ fg bg) -> BGText t fg bg

drawItemInInventory :: Position -> Item -> TileImage -> TileImage
drawItemInInventory pos item = drawText pos txt
    where 
        txt = Icon itemDrawable <> toText (" " ++ itemTxt) <> FGText "X" grey
        itemTxt = rightPad (sidebarSize - 4 - 2 - 1) ' ' $ show item
        toText t = FGText t grey
        itemDrawable = Drawable itemGlyph grey
        itemGlyph = case lookupItemDrawable item of
            (Drawable g _) -> g
            (DrawableBG g _ _) -> g

leftPad :: Int -> a -> [a] -> [a]
leftPad m x xs = replicate (m - length xs) x ++ xs

rightPad :: Int -> a -> [a] -> [a]
rightPad m x xs = take m $ xs ++ repeat x

drawUI :: TileImage -> System' TileImage
drawUI tm = drawLog tm 
    >>= drawSidebarBG 
    >>= drawShoppingListBG 
    >>= drawShoppingListHeader 
    >>= drawShoppingListContent 
    >>= drawInventoryHeader 
    >>= drawInventoryContent