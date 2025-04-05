{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Pages.Exercises (exercisesPage) where

-- Imports needed by the Focail logic

import Control.Lens ((.~), (^.)) -- Added (.~) for textInputConfig
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Javascript.JSaddle (eval, liftJSM) -- Needed for JSM effects
import Reflex.Dom

-- Imports for the structure
import App.Types
import qualified Data.Map as Map
import UI.Common (css, tShow)

-- --- Static Data (Moved outside) ---
sentencePairs :: [(Text, Text, Text, Text)]
sentencePairs =
    [ ("I would like to speak Irish with my friends", "Ba mhaith liom Gaeilge a labhairt le mo chairde", "Verb: labhairt (to speak)", "abairt1.wav")
    , ("I am learning Irish every day", "Tá mé ag foghlaim Gaeilge gach lá", "Verb: foghlaim (to learn)", "abairt2.wav")
    , ("Can you help me with my homework?", "An féidir leat cabhrú liom le mo obair bhaile?", "Verb: cabhrú (to help)", "abairt3.wav")
    , ("What time is it now?", "Cén t-am é anois?", "Verb: is (to be)", "abairt4.wav")
    , ("I like to read books in Irish", "Is maith liom leabhair a léamh as Gaeilge", "Verb: léamh (to read)", "abairt5.wav")
    , ("I say hello to everyone", "Deirim hello le gach duine", "Verb: abair (to say)", "abairt6.wav")
    , -- ... include ALL sentence pairs here ...
      ("I get tired in the evening", "Tuirsím tráthnóna", "Verb: tuirsigh (to tire)", "abairt1.wav")
    , ("I ask the teacher a question", "Fiafraím den mhúinteoir ceist", "Verb: fiafraigh (to ask/enquire)", "abairt1.wav")
    ]

-- --- Page Widget (Wrapping Focail Logic) ---
exercisesPage :: OpenActionWidget t () -- Signature remains correct
exercisesPage appEnv () = do
    -- Overall page container - capture nav event from it
    navEvent <- elAttr "div" (css "page page-exercises") $ do
        -- Focail App Header
        _ <- elAttr "div" (css "focail-header") $ do
            -- Handle unused bind
            elAttr "img" (Map.fromList [("src", "./fox.jpeg"), ("alt", "Fox Icon"), ("class", "focail-app-icon")]) blank
            el "h1" $ text "Focail"
            el "p" $ text "Irish Verb Learning"

        -- Main Focail Content Area
        _ <- elAttr "div" (css "focail-content") $ mdo
            -- Handle unused bind
            let total = length sentencePairs
            prevBtn <- button "← Previous"
            nextBtn <- button "Next →"
            let prevEvent = (\i -> max 0 (i - 1)) <$ prevBtn
            let nextEvent = (\i -> min (total - 1) (i + 1)) <$ nextBtn
            (doneEventTrig, doneTrigAct) <- newTriggerEvent
            currentIndex <- foldDyn ($) 0 $ leftmost [prevEvent, nextEvent, doneEventTrig]

            _ <-
                elAttr "div" (css "focail-position-display") $
                    dynText $
                        fmap (\i -> T.pack $ "Sentence " <> show (i + 1) <> " of " <> show total) currentIndex

            let currentSentenceDyn = fmap (sentencePairs !!) currentIndex
            let englishPromptDyn = fmap (\(eng, _, _, _) -> eng) currentSentenceDyn
            let irishHintDyn = fmap (\(_, irish, _, _) -> irish) currentSentenceDyn
            let verbInfoDyn = fmap (\(_, _, verb, _) -> verb) currentSentenceDyn
            let sentenceAudioDyn = fmap (\(_, _, _, audio) -> audio) currentSentenceDyn

            _ <- elAttr "div" (css "focail-prompt") $ do
                el "h3" $ text "Translate to Irish:"
                el "p" $ dynText englishPromptDyn

            _ <- elAttr "div" (css "focail-input-area") $ do
                rec ti <-
                        textInput $
                            def
                                & textInputConfig_attributes
                                .~ constDyn (css "focail-input" <> ("placeholder" =: "Type your answer in Irish..."))
                                    & textInputConfig_setValue
                                .~ ("" <$ updated currentIndex)

                    let inputDyn = _textInput_value ti
                    let keyPressEvent = updated inputDyn

                    performEvent_ $ ffor keyPressEvent $ \_ -> liftJSM $ do
                        _ <- eval $ T.pack "new Audio('key-press.mp3').play()"
                        return ()

                    let targetWordsDyn = fmap T.words irishHintDyn

                    let maskSentenceDyn =
                            zipDynWith
                                ( \input hint ->
                                    let inputChars = T.unpack input
                                        hintChars = T.unpack hint
                                        maskChar i h
                                            | i == h || toLower i == toLower h = h
                                            | h == ' ' = ' '
                                            | otherwise = '_'
                                     in T.pack $ zipWith maskChar (inputChars ++ repeat ' ') hintChars
                                )
                                inputDyn
                                irishHintDyn

                    -- \*** FORMATTED currentWordInfoDyn ***
                    let currentWordInfoDyn =
                            zipDynWith
                                ( \input targetWords ->
                                    let
                                        inputWords = T.words input
                                        currentWordIndex = min (max 0 (length inputWords - 1)) (length targetWords - 1)
                                        targetWord = if null targetWords then T.empty else targetWords !! currentWordIndex
                                        typedWord = if not (null inputWords) then last inputWords else T.empty
                                        targetStr = T.unpack targetWord
                                        typedStr = T.unpack typedWord

                                        charsMatch :: Char -> Char -> (Bool, Bool)
                                        charsMatch typed target =
                                            if typed == target || toLower typed == toLower target
                                                then (True, False) -- Exact match
                                                else case (toLower typed, toLower target) of
                                                    ('a', 'á') -> (True, True) -- Accent match
                                                    ('e', 'é') -> (True, True)
                                                    ('i', 'í') -> (True, True)
                                                    ('o', 'ó') -> (True, True)
                                                    ('u', 'ú') -> (True, True)
                                                    _ -> (False, False) -- No match
                                        charMatches :: [(Bool, Bool)]
                                        charMatches =
                                            [ if i < length typedStr
                                                then charsMatch (typedStr !! i) tc
                                                else (False, False)
                                            | (i, tc) <- zip [0 ..] targetStr
                                            ]

                                        isCurrentWordComplete :: Bool
                                        isCurrentWordComplete =
                                            length typedStr >= length targetStr && all fst charMatches

                                        renderChar :: (Int, Char) -> String
                                        renderChar (i, targetChar) =
                                            if i < length typedStr
                                                then
                                                    let
                                                        typedChar = typedStr !! i
                                                        (isMatch, isAccent) = charsMatch typedChar targetChar
                                                        style =
                                                            if isMatch && not isAccent
                                                                then "color: green; background-color: #e8f5e9;"
                                                                else
                                                                    if isMatch && isAccent
                                                                        then "color: #b7860b; background-color: #fff8e1;" -- Yellow for accent
                                                                        else "color: red; background-color: #ffebee;"
                                                     in
                                                        "<span style=\"" ++ style ++ "\">" ++ [typedChar] ++ "</span>"
                                                else
                                                    "<span>_</span>"

                                        html :: Text
                                        html =
                                            T.pack $
                                                "<div style=\"font-family: monospace;\">"
                                                    ++ concatMap renderChar (zip [0 ..] targetStr)
                                                    ++ "</div>"
                                     in
                                        (targetWord, html, isCurrentWordComplete) -- Return tuple
                                )
                                inputDyn
                                targetWordsDyn

                    let currentTargetWordDyn = fmap (\(word, _, _) -> word) currentWordInfoDyn
                    let colorWordDyn = fmap (\(_, html, _) -> html) currentWordInfoDyn
                    let isCompleteDyn = fmap (not . T.any (== '_')) maskSentenceDyn
                    let correctnessAchievedEvent = ffilter id (updated isCompleteDyn)

                    delayedCorrectEvent <- delay 0.5 correctnessAchievedEvent

                    performEvent_ $ ffor delayedCorrectEvent $ \_ -> liftIO $ doneTrigAct $ \i -> min (total - 1) (i + 1)

                    let taggedAudioEvent = tag (current sentenceAudioDyn) correctnessAchievedEvent

                    performEvent_ $ ffor taggedAudioEvent $ \audioPath -> liftJSM $ do
                        let jsStringToExecute = T.pack "new Audio('ping.mp3').play(); new Audio('" <> T.replace "'" "\\'" audioPath <> T.pack "').play()"
                        _ <- eval jsStringToExecute
                        return ()

                    playTTS <- button "Hear the Irish spoken"
                    let hintAudioEvent = tag (current sentenceAudioDyn) playTTS

                    performEvent_ $ ffor hintAudioEvent $ \audioPath -> liftJSM $ do
                        let jsStringToExecute = T.pack "new Audio('" <> T.replace "'" "\\'" audioPath <> T.pack "').play()"
                        _ <- eval jsStringToExecute
                        return ()

                    hb <- button "Show Word Hint"
                    hintVisible <- toggle False hb

                -- End of inner 'rec' block

                _ <- widgetHold blank $ ffor (updated $ zipDyn hintVisible currentTargetWordDyn) $ \(visible, word) ->
                    if visible then elAttr "div" (css "focail-hint") $ text $ "Current word: " <> word else blank

                _ <- widgetHold blank $ ffor (updated $ zipDyn isCompleteDyn maskSentenceDyn) $ \(isComplete, maskText) ->
                    if isComplete then elAttr "div" (css "focail-feedback-correct") $ text $ "Correct! " <> maskText else blank

                _ <- elAttr "div" (css "focail-current-word") $ do
                    el "p" $ text "Current word:"
                    elDynHtml' "div" colorWordDyn

                _ <- elAttr "div" (css "focail-progress-mask") $ do
                    el "p" $ text "Your progress:"
                    dynText maskSentenceDyn

                _ <-
                    elAttr "div" (css "focail-verb-info") $
                        dynText verbInfoDyn

                pure () -- End of input area do block
            pure () -- End of focail content do block

        -- \*** ADDED Navigation Section at the bottom ***
        navEventFromSection <- elAttr "div" (css "navigation-section") $ do
            el "h2" $ text "Navigation"
            homeBtn <- button "Return Home"
            pure (MyHome <$ homeBtn) -- Return event mapped to MyHome
        pure navEventFromSection -- Return captured nav event from the main page div

    -- Construct AppEvents using only the captured navigation event
    pure $ mempty{_ae_navigation = navEvent}
