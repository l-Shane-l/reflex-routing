{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Not strictly needed here now, but harmless
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Lens ((^.))
import Control.Monad.Fix (MonadFix)
import Reflex.Dom hiding (mainWidget) -- Avoid ambiguity
import Reflex.Dom.Core (mainWidgetWithHead) -- Use specific import

-- Project Modules

import App.Router (routeToWidget)
import App.Types
import UI.Common (css, headWidget) -- Import headWidget

main :: IO ()
main = Reflex.Dom.mainWidgetWithHead headWidget $ -- Use mainWidgetWithHead
    do
        -- Top-level structure with CSS classes
        elAttr "div" (css "app-container") $ do
            elAttr "header" (css "app-header") $ do
                el "h1" $ text "My Reflex App (Modular)" -- Updated title
            elAttr "main" (css "app-content") $ do
                let initialRoute = MyHome
                let initialCounter = 0
                rec let appEnv = AppEnv{_env_appState = appState}
                    let appState = AppState{_appState_route = currentRouteDyn, _appState_counter = counterDyn}

                    currentRouteDyn <- holdDyn initialRoute _navigationEvent
                    counterDyn <- foldDyn ($) initialCounter _counterUpdateEvent

                    -- Use imported routeToWidget - let GHC infer type here
                    let currentPageAction = App.Router.routeToWidget appEnv

                    pageEventsDyn :: Dynamic t (AppEvents t) <- -- Signature helps widgetHold
                        widgetHold (currentPageAction initialRoute) (currentPageAction <$> updated currentRouteDyn)

                    -- Correct Event Extraction (as before)
                    let pageEventsBehavior = current pageEventsDyn
                    let navigationEventBehavior = fmap _ae_navigation pageEventsBehavior
                    let counterUpdateEvBehavior = fmap _ae_counterUpdate pageEventsBehavior
                    let _navigationEvent = switch navigationEventBehavior
                    let _counterUpdateEvent = switch counterUpdateEvBehavior
                pure ()
        pure ()
