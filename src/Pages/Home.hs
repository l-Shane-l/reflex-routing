{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Pages.Home (homePage) where

import Control.Lens ((^.))
import Data.Map (Map) -- Import Map for attributes
import qualified Data.Map as Map
import Data.Text (Text)
import Reflex.Dom

import App.Types
import UI.Common (css)

-- --- Page Widget ---
homePage :: OpenActionWidget t () -- Takes AppEnv, input (), returns AppEvents
homePage _appEnv () = do
    -- Capture navigation events from the relevant section
    navEvent <- elAttr "div" (css "page page-home") $ do
        -- Welcome Section
        _ <- elAttr "div" (css "welcome-section") $ do
            -- Handled unused bind
            el "h1" $ text "Welcome to Focail / Do Ghairdín!"
            el "p" $ text "Learn Irish verbs and grow your language garden."

        -- Action Buttons Section
        navEventFromButtons <- elAttr "div" (css "action-buttons") $ do
            -- Handled unused bind
            -- Button 1: Go to Exercises
            exBtn <- button "50 Verb Exercises"
            -- Button 2: Go to My Garden
            gardenBtn <- button "My Garden"
            -- Return events for both buttons
            pure $
                leftmost
                    [ Exercises <$ exBtn
                    , Garden <$ gardenBtn -- Navigate to Garden route
                    ]

        -- Return the combined navigation event from this page
        pure navEventFromButtons

    -- Construct the AppEvents record using only the navigation event
    pure $ mempty{_ae_navigation = navEvent}
