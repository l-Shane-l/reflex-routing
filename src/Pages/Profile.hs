{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Pages.Profile (profilePage) where

import Control.Lens ((^.))
import Data.Text (Text)
import Reflex.Dom

import App.Types
import UI.Common (css)

-- --- Page Widget ---
profilePage :: OpenActionWidget t ()
profilePage _appEnv () = do
    navEvent <- elAttr "div" (css "page page-profile") $ do
        el "h1" $ text "Profile Page"
        el "p" $ text "This page is simpler. It doesn't display or interact with the shared counter state."
        el "p" $ text "It only provides navigation back to other pages."

        navigationClickEvent <- elAttr "div" (css "navigation-section") $ do
            el "h2" $ text "Navigation"
            homeBtn <- button "Go Home"
            aboutBtn <- button "Go to About"
            pure $ leftmost [MyHome <$ homeBtn, About <$ aboutBtn]
        pure navigationClickEvent

    pure $ mempty{_ae_navigation = navEvent}
