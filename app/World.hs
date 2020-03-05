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
module World where
import Apecs hiding (Map, Set)
import qualified Apecs as Apecs
import Linear
import Position
import Data.Array
import CDrawable
import Item
import Apecs.Experimental.Reactive
import TerminalText

newtype CPosition = CPosition Position 
    deriving (Show, Eq, Ord)
    deriving (Ix) via Position
instance Component CPosition where type Storage CPosition = Reactive (IxMap CPosition) (Apecs.Map CPosition)
instance Bounded CPosition where
    minBound = CPosition (V2 0 0)
    maxBound = CPosition (V2 1000 1000)

data CPlayer = CPlayer deriving Show
instance Component CPlayer where type Storage CPlayer = Unique CPlayer

instance Component CDrawable where type Storage CDrawable = Apecs.Map CDrawable

data CItem = CItem Item
instance Component CItem where type Storage CItem = Apecs.Map CItem

data CSolid = CSolid
instance Component CSolid where type Storage CSolid = Apecs.Map CSolid

data CInventory = CInventory [Item]
instance Component CInventory where type Storage CInventory = Apecs.Map CInventory

newtype CActions = CActions [Action]
    deriving (Monoid, Semigroup) via ([Action])
instance Component CActions where type Storage CActions = Apecs.Global CActions

data CShoppingList = CShoppingList [Item]
instance Component CShoppingList where type Storage CShoppingList = Apecs.Map CShoppingList

data Behaviour = Buy Item

newtype CBehaviour = CBehaviour Behaviour
instance Component CBehaviour where type Storage CBehaviour = Apecs.Map CBehaviour

data Action = Move Direction

data Direction = DirUp | DirDown | DirLeft | DirRight

newtype CLog = CLog [TerminalText]
    deriving (Monoid, Semigroup) via ([TerminalText])
instance Component CLog where type Storage CLog = Apecs.Global CLog

newtype CTime = CTime Float
instance Component CTime where type Storage CTime = Apecs.Global CTime
instance Semigroup CTime where (CTime t1) <> (CTime t2) = CTime (t1 + t2)
instance Monoid CTime where mempty = CTime 0

newtype CName = CName String
instance Component CName where type Storage CName = Apecs.Map CName

makeWorld "World" [''CPosition, ''CPlayer, ''CDrawable, ''CSolid, ''CItem, ''CInventory, ''CTime, ''CActions, ''CBehaviour, ''CLog, ''CName, ''CShoppingList]

destroyEntity :: Entity -> System' () 
destroyEntity e = destroy e (Proxy :: Proxy (CPosition, CPlayer, CDrawable, CSolid, CItem, CInventory, CBehaviour, CName))

type System' a = System World a