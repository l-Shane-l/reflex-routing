{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Pages.About (aboutPage) where

import Control.Lens ((^.))
import Data.Text (Text)
import Reflex.Dom

import App.Types
import UI.Common (css, tShow)

-- --- Page Widget ---
aboutPage :: OpenActionWidget t ()
aboutPage appEnv () = do
    let counterDyn = appEnv ^. env_appState . appState_counter
    (navEvent, counterUpdateEv) <- elAttr "div" (css "page page-about") $ do
        el "h1" $ text "About Page"
        el "p" $ text "This application showcases a structured approach to building Reflex applications using:"
        el "ul" $ do el "li" $ text "Interface Records (`AppState`, `AppEnv`, `AppEvents`)"; el "li" $ text "Semantic Type Aliases (`OpenActionWidget`, etc.)"; el "li" $ text "Explicit Environment Passing"; el "li" $ text "A Central Wiring `rec` Block"

        incButtonClickEvent <- elAttr "div" (css "counter-section") $ do
            el "h2" $ text "Shared Counter"
            el "p" $ do text "Current count: "; elAttr "strong" (css "counter-value") $ dynText (tShow <$> counterDyn)
            button "Increment Counter"

        navigationClickEvent <- elAttr "div" (css "navigation-section") $ do
            el "h2" $ text "Navigation"
            homeBtn <- button "Go Home"
            profileBtn <- button "Go to Profile"
            pure $ leftmost [MyHome <$ homeBtn, Profile <$ profileBtn]

        let counterUpdateAction = (+ 1) <$ incButtonClickEvent
        pure (navigationClickEvent, counterUpdateAction)

    pure $
        AppEvents
            { _ae_navigation = navEvent
            , _ae_counterUpdate = counterUpdateEv
            }
