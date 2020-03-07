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
module Interaction (interaction, interaction_) where
import Control.Monad
import Apecs

interaction_ :: forall w m cx cy a. (Get w m cx, Members w m cx, Get w m cy, Members w m cy) => Entity -> Entity -> (cx -> cy -> SystemT w m a) -> SystemT w m ()
interaction_ e1 e2 f = interaction e1 e2 f >> return ()

interaction :: forall w m cx cy a. (Get w m cx, Members w m cx, Get w m cy, Members w m cy) => Entity -> Entity -> (cx -> cy -> SystemT w m a) -> SystemT w m (Maybe a)
interaction e1 e2 f = join <$> (doIfPossible' e1 (\c -> doIfPossible' e2 (f c)))

doIfPossible' :: forall w m cx a. (Get w m cx, Members w m cx) => Entity -> (cx -> SystemT w m a) -> SystemT w m (Maybe a)
doIfPossible' e f = doIfPossible e f Proxy

doIfPossible :: forall w m cx a. (Get w m cx, Members w m cx) => Entity -> (cx -> SystemT w m a) -> Proxy cx -> SystemT w m (Maybe a)
doIfPossible e f p = do
    ex <- exists e p
    if ex then do
        cx <- get e
        Just <$> f cx
    else return Nothing
