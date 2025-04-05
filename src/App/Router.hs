{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module App.Router (routeToWidget) where

import Reflex.Dom.Class -- Only need MonadWidget constraint

import App.Types -- Core types and aliases
import Pages.About (aboutPage)
import Pages.Home (homePage)
import Pages.Profile (profilePage)
import Reflex.Dom

-- Route-to-Widget mapping function
-- Signature specifies MonadWidget requirement
routeToWidget :: (MonadWidget t m) => AppEnv t -> Route -> m (AppEvents t)
routeToWidget appEnv route = case route of
    MyHome -> homePage appEnv ()
    About -> aboutPage appEnv ()
    Profile -> profilePage appEnv ()
