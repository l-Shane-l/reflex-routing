{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Lens (makeLenses, (^.))
import Control.Monad.Fix (MonadFix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom hiding (mainWidget) -- Hide mainWidget to use mainWidgetWithHead
import Reflex.Dom.Core (mainWidgetWithHead) -- Import explicitly

-- Helper
tShow :: (Show a) => a -> Text
tShow = T.pack . show

-- Data Types (As before)
data Route = MyHome | About | Profile deriving (Eq, Show, Read, Ord)
type CounterUpdate = Int -> Int
data AppState t = AppState {_appState_route :: Dynamic t Route, _appState_counter :: Dynamic t Int}
makeLenses ''AppState
data AppEnv t = AppEnv {_env_appState :: AppState t}
makeLenses ''AppEnv
data AppEvents t = AppEvents {_ae_navigation :: Event t Route, _ae_counterUpdate :: Event t CounterUpdate}
makeLenses ''AppEvents
instance (Reflex t) => Semigroup (AppEvents t) where a <> b = AppEvents (leftmost [a ^. ae_navigation, b ^. ae_navigation]) (leftmost [a ^. ae_counterUpdate, b ^. ae_counterUpdate])
instance (Reflex t) => Monoid (AppEvents t) where mempty = AppEvents never never

-- Helper for CSS classes
css :: Text -> Map Text Text
css className = ("class" =: className)

-- --- CSS Rules Defined as Text ---
-- Colocating here for simplicity, could be near widgets or in a dedicated module
appStyles :: Text
appStyles =
    T.unlines
        [ "body { font-family: sans-serif; margin: 0; padding: 0; background-color: #f9f9f9; }"
        , ".app-container { max-width: 960px; margin: 20px auto; background-color: #fff; padding: 20px; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); }"
        , ".app-header { margin-bottom: 20px; padding-bottom: 15px; border-bottom: 1px solid #eee; }"
        , ".app-header h1 { color: #333; margin: 0; text-align: center; }"
        , ".app-content { }"
        , ".page { margin-top: 20px; padding: 15px; border: 1px solid #ddd; border-radius: 4px; background-color: #fff; }"
        , ".page h1 { margin-top: 0; color: #444; }"
        , ".counter-section, .navigation-section { margin-top: 20px; padding-top: 15px; border-top: 1px dashed #eee; }"
        , ".counter-section h2, .navigation-section h2 { margin-top: 0; font-size: 1.1em; color: #555; }"
        , ".counter-value { font-weight: bold; color: blue; font-size: 1.2em; margin-left: 5px; }"
        , "button { padding: 8px 15px; margin-right: 10px; cursor: pointer; background-color: #eee; border: 1px solid #ccc; border-radius: 3px; font-size: 0.95em; }"
        , "button:hover { background-color: #ddd; }"
        , "ul { padding-left: 20px; }"
        , "li { margin-bottom: 5px; }"
        ]

-- --- Widget for the <head> ---
headWidget :: (MonadWidget t m) => m ()
headWidget = do
    el "title" $ text "Reflex Structured App"
    -- Embed the CSS within a <style> tag
    el "style" $ text appStyles

-- --- Role-Based Type Aliases (As before) ---
type InsularViewWidget t input output = forall m. (MonadWidget t m) => input -> m output
type InsularActionWidget t input = forall m. (MonadWidget t m) => input -> m (AppEvents t)
type OpenViewWidget t input output = forall m. (MonadWidget t m) => AppEnv t -> input -> m output
type OpenActionWidget t input = forall m. (MonadWidget t m) => AppEnv t -> input -> m (AppEvents t)

homePage :: OpenActionWidget t ()
homePage appEnv () = do
    let counterDyn = appEnv ^. env_appState . appState_counter

    -- Create elements and capture events separately
    (navEvent, counterUpdateEv) <- elAttr "div" (css "page page-home") $ do
        el "h1" $ text "Home Page"
        el "p" $ text "Welcome to the Reflex architectural demo application!"
        el "p" $ do text "This page demonstrates accessing shared state (the counter below) and triggering navigation."

        -- Counter Section: Output the increment event
        incButtonClickEvent <- elAttr "div" (css "counter-section") $ do
            el "h2" $ text "Shared Counter"
            el "p" $ do text "Current count: "; elAttr "strong" (css "counter-value") $ dynText (tShow <$> counterDyn)
            button "Increment Counter"

        -- Navigation Section: Output the navigation event
        navigationClickEvent <- elAttr "div" (css "navigation-section") $ do
            el "h2" $ text "Navigation"
            aboutBtn <- button "Go to About Page"
            profileBtn <- button "Go to Profile Page"
            pure $ leftmost [About <$ aboutBtn, Profile <$ profileBtn]

        -- Combine results from sections
        let counterUpdateAction = (+ 1) <$ incButtonClickEvent
        pure (navigationClickEvent, counterUpdateAction) -- Return tuple of events

    -- Construct the final AppEvents record
    pure $
        AppEvents
            { _ae_navigation = navEvent
            , _ae_counterUpdate = counterUpdateEv
            }

aboutPage :: OpenActionWidget t ()
aboutPage appEnv () = do
    let counterDyn = appEnv ^. env_appState . appState_counter

    (navEvent, counterUpdateEv) <- elAttr "div" (css "page page-about") $ do
        el "h1" $ text "About Page"
        el "p" $ text "This application showcases a structured approach..." -- Truncated for brevity
        el "ul" $ do el "li" $ text "..."; el "li" $ text "..."; el "li" $ text "..."; el "li" $ text "..." -- Truncated

        -- Counter Section
        incButtonClickEvent <- elAttr "div" (css "counter-section") $ do
            el "h2" $ text "Shared Counter"
            el "p" $ do text "Current count: "; elAttr "strong" (css "counter-value") $ dynText (tShow <$> counterDyn)
            button "Increment Counter"

        -- Navigation Section
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

profilePage :: OpenActionWidget t ()
profilePage _appEnv () = do
    elAttr "div" (css "page page-profile") $ do
        el "h1" $ text "Profile Page"
        el "p" $ text "This page is simpler. It doesn't display or interact with the shared counter state."
        el "p" $ text "It only provides navigation back to other pages."
        elAttr "div" (css "navigation-section") $ do
            el "h2" $ text "Navigation"
            homeBtn <- button "Go Home"
            aboutBtn <- button "Go to About"
            let navEvent = leftmost [MyHome <$ homeBtn, About <$ aboutBtn]
            pure $ mempty{_ae_navigation = navEvent}

-- Route-to-Widget mapping function (remains the same)
routeToWidget :: (MonadWidget t m) => AppEnv t -> Route -> m (AppEvents t)
routeToWidget appEnv route = case route of MyHome -> homePage appEnv (); About -> aboutPage appEnv (); Profile -> profilePage appEnv ()

-- --- Application Logic (within main function body) ---
appBody :: (MonadWidget t m) => m () -- Extracted body logic into a function
appBody = do
    elAttr "div" (css "app-container") $ do
        elAttr "header" (css "app-header") $ do
            el "h1" $ text "My Reflex App (Embedded CSS)" -- Updated title
        elAttr "main" (css "app-content") $ do
            let initialRoute = MyHome
            let initialCounter = 0
            rec let appEnv = AppEnv{_env_appState = appState}
                let appState = AppState{_appState_route = currentRouteDyn, _appState_counter = counterDyn}

                currentRouteDyn <- holdDyn initialRoute _navigationEvent
                counterDyn <- foldDyn ($) initialCounter _counterUpdateEvent

                let currentPageAction route = routeToWidget appEnv route

                pageEventsDyn :: Dynamic t (AppEvents t) <-
                    widgetHold (currentPageAction initialRoute) (currentPageAction <$> updated currentRouteDyn)

                let pageEventsBehavior = current pageEventsDyn
                let navigationEventBehavior = fmap _ae_navigation pageEventsBehavior
                let counterUpdateEvBehavior = fmap _ae_counterUpdate pageEventsBehavior
                let _navigationEvent = switch navigationEventBehavior
                let _counterUpdateEvent = switch counterUpdateEvBehavior
            pure ()
    pure ()

-- --- Main Function (Using mainWidgetWithHead) ---
main :: IO ()
main = Reflex.Dom.mainWidgetWithHead headWidget appBody -- Pass head and body widgets
