{-# LANGUAGE OverloadedStrings #-}

module UI.Common (
    tShow,
    css,
    appStyles,
    headWidget,
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom -- Needed for el, text, MonadWidget

-- Helper to show Text
tShow :: (Show a) => a -> Text
tShow = T.pack . show

-- Helper for CSS classes
css :: Text -> Map Text Text
css className = ("class" =: className)

-- --- CSS Rules Defined as Text ---
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
    el "style" $ text appStyles
