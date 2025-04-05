{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module App.Router (routeToWidget) where

import Reflex.Dom.Class

import App.Types
import Pages.About (aboutPage)
import Pages.Exercises (exercisesPage)
import Pages.Garden (gardenPage) -- <<< Import Garden page
import Pages.Home (homePage)
import Pages.Profile (profilePage)
import Reflex.Dom

-- Route-to-Widget mapping function
routeToWidget :: (MonadWidget t m) => AppEnv t -> Route -> m (AppEvents t)
routeToWidget appEnv route = case route of
    MyHome -> homePage appEnv ()
    About -> aboutPage appEnv ()
    Profile -> profilePage appEnv ()
    Exercises -> exercisesPage appEnv ()
    Garden -> gardenPage appEnv ()

-- Since gardenPage is a ViewWidget returning (), we explicitly return mempty AppEvents after running it.
