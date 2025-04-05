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
import Reflex.Dom

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
        , -- Styles from Focail App (mapped to new classes)
          ".focail-header { background-color: #2e7d32; color: white; padding: 20px; text-align: center; }" -- Was headerStyle
        , ".focail-app-icon { height: 60px; margin-bottom: 10px; border-radius: 8px; }" -- Was appIconStyle
        , ".focail-content { max-width: 600px; margin: 20px auto; padding: 0; }" -- Adjusted from contentStyle to avoid conflict
        , ".focail-prompt { background-color: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-bottom: 20px; }" -- Was promptStyle
        , ".focail-prompt p { font-size: 18px; }"
        , ".focail-input-area { background-color: #f5f5f5; padding: 20px; border-radius: 8px; }" -- Was inputAreaStyle
        , ".focail-input { width: calc(100% - 24px); padding: 12px; margin: 10px 0; font-size: 16px; border: 1px solid #ddd; border-radius: 4px; }" -- Adjusted width from inputStyle
        , ".focail-hint { color: #757575; margin: 10px 0; font-style: italic; background-color: #f9f9f9; padding: 8px; border-radius: 4px; border-left: 3px solid #2e7d32; }" -- Was hintStyle
        , ".focail-feedback-correct { color: #2e7d32; font-weight: bold; margin-top: 10px; padding: 10px; background-color: #e8f5e9; border-radius: 4px; }" -- Was correctStyle
        -- Add .focail-feedback-incorrect if needed
        , ".focail-verb-info { margin-top: 20px; padding-top: 10px; border-top: 1px solid #ddd; color: #616161; font-size: 14px; }" -- Was verbInfoStyle
        , ".focail-progress-mask { font-family: monospace; font-size: 18px; background: #f0f0f0; padding: 10px; border-radius: 4px; letter-spacing: 2px; margin: 15px 0; }"
        , ".focail-current-word { font-family: monospace; font-size: 24px; letter-spacing: 3px; margin: 10px 0; }"
        , ".focail-current-word div { font-family: monospace; }" -- Ensure inner div inherits font
        , ".focail-position-display { text-align: center; margin: 10px 0; }"
        , ".page-home .welcome-section { text-align: center; margin-bottom: 30px; }"
        , ".page-home .welcome-section h1 { font-size: 2.5em; color: #1b5e20; margin-bottom: 10px; }" -- Darker green
        , ".page-home .welcome-section p { font-size: 1.2em; color: #555; }"
        , ".page-home .action-buttons { display: flex; justify-content: center; gap: 20px; margin-bottom: 40px; }"
        , ".page-home .action-buttons button { background-color: #4caf50; color: white; border: none; font-size: 1.1em; padding: 15px 30px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.2); }"
        , ".page-home .action-buttons button:hover { background-color: #388e3c; }"
        , ".page-home .garden-section { text-align: center; background-color: #f1f8e9; padding: 20px; border-radius: 8px; border: 1px solid #dcedc8; }" -- Light green background
        , ".page-home .garden-section h2 { color: #33691e; }" -- Darker green
        , ".page-home .garden-section img { max-width: 150px; height: auto; margin: 15px 0; display: block; margin-left: auto; margin-right: auto; }" -- Style for the placeholder GIF
        , ".page-home .garden-section .progress-text { font-size: 1.1em; color: #333; }"
        , ".page-home .garden-section .progress-number { font-size: 1.8em; font-weight: bold; color: #2e7d32; }" -- Match Focail header green
        ]

-- --- Widget for the <head> ---
headWidget :: (MonadWidget t m) => m ()
headWidget = do
    el "title" $ text "Reflex Structured App"
    el "style" $ text appStyles
