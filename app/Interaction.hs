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
module Interaction (interaction) where
import Control.Monad
import Apecs

interaction :: forall w m cx cy. (Get w m cx, Members w m cx, Get w m cy, Members w m cy) => Entity -> Entity -> (cx -> cy -> SystemT w m ()) -> SystemT w m ()
interaction e1 e2 f = do
    doIfPossible' e1 $ \c -> doIfPossible' e2 (f c)

doIfPossible' :: forall w m cx. (Get w m cx, Members w m cx) => Entity -> (cx -> SystemT w m ()) -> SystemT w m ()
doIfPossible' e f = doIfPossible e f Proxy

doIfPossible :: forall w m cx. (Get w m cx, Members w m cx) => Entity -> (cx -> SystemT w m ()) -> Proxy cx -> SystemT w m ()
doIfPossible e f p = do
    ex <- exists e p
    when ex $ do
        cx <- get e
        f cx
