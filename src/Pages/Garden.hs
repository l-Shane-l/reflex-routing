{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Pages.Garden (gardenPage) where

import Control.Lens ((^.)) -- Keep for potential future use
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Reflex.Dom

import App.Types -- Uses types and aliases defined here
import UI.Common (css)

-- --- Page Widget ---
-- Corrected Signature: Uses OpenActionWidget with only t and input ()
gardenPage :: OpenActionWidget t ()
gardenPage _appEnv () = do
    -- Takes AppEnv, Takes (), returns m (AppEvents t)

    -- Capture navigation event from the page
    navEvent <- elAttr "div" (css "page page-garden") $ do
        el "h1" $ text "My Language Garden"

        -- My Garden Section (Static content)
        _ <- elAttr "div" (css "garden-section") $ do
            -- Handle unused bind
            el "h2" $ text "Your Progress"
            _ <-
                elAttr
                    "img"
                    ( Map.fromList
                        [ ("src", "./output.gif")
                        , ("alt", "Your growing language tree")
                        ]
                    )
                    blank
            _ <- elAttr "p" (css "progress-text") $ do
                -- Handle unused bind
                text "You have learned "
                elAttr "span" (css "progress-number") $ text "42"
                text " words!"
            pure ()

        -- Navigation Section
        navEventFromSection <- elAttr "div" (css "navigation-section") $ do
            -- Handle unused bind
            el "h2" $ text "Navigation"
            homeBtn <- button "Back to Home"
            pure (MyHome <$ homeBtn) -- Return the event mapped to MyHome route
        pure navEventFromSection -- Return event from the main element block

    -- Construct AppEvents with the captured navigation event
    pure $ mempty{_ae_navigation = navEvent}
