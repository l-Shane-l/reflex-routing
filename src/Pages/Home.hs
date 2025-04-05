-- For MonadWidget constraint implicit in alias
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- For MonadWidget constraint implicit in alias
{-# LANGUAGE TypeFamilies #-}

module Pages.Home (homePage) where

import Control.Lens ((^.))
import Data.Text (Text)
import Reflex.Dom

import App.Types -- Import shared types and aliases
import UI.Common (css, tShow) -- Import helpers

-- --- Page Widget ---
homePage :: OpenActionWidget t () -- Use the alias from App.Types
homePage appEnv () = do
    let counterDyn = appEnv ^. env_appState . appState_counter
    (navEvent, counterUpdateEv) <- elAttr "div" (css "page page-home") $ do
        el "h1" $ text "Home Page"
        el "p" $ text "Welcome to the Reflex architectural demo application!"
        el "p" $ do text "This page demonstrates accessing shared state (the counter below) and triggering navigation."

        incButtonClickEvent <- elAttr "div" (css "counter-section") $ do
            el "h2" $ text "Shared Counter"
            el "p" $ do text "Current count: "; elAttr "strong" (css "counter-value") $ dynText (tShow <$> counterDyn)
            button "Increment Counter"

        navigationClickEvent <- elAttr "div" (css "navigation-section") $ do
            el "h2" $ text "Navigation"
            aboutBtn <- button "Go to About Page"
            profileBtn <- button "Go to Profile Page"
            pure $ leftmost [About <$ aboutBtn, Profile <$ profileBtn]

        let counterUpdateAction = (+ 1) <$ incButtonClickEvent
        pure (navigationClickEvent, counterUpdateAction)

    pure $
        AppEvents
            { _ae_navigation = navEvent
            , _ae_counterUpdate = counterUpdateEv
            }
