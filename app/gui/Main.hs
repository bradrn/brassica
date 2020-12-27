{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import SoundChange
import SoundChange.Parse

main :: IO ()
main = startGUI defaultConfig { jsCustomHTML = Just "gui.html", jsStatic = Just "static" } setup

setup :: Window -> UI ()
setup window = do
    cats     <- getElementById' "categories"
    rules    <- getElementById' "rules"
    words    <- getElementById' "words"
    applyBtn <- getElementById' "applyBtn"
    out      <- getElementById' "out"

    on UI.click applyBtn $ const $ do
        catsText  <- lines <$> get value cats
        rulesText <- lines <$> get value rules
        wordsText <- lines <$> get value words

        let cats = parseCategoriesSpec catsText
            rules = parseRules cats rulesText
            results = tokeniseAndApplyRules cats rules <$> wordsText

        element out # set value (unlines results)
  where
    getElementById' i = getElementById window i >>= \case
        Nothing -> error "Tried to get nonexistent ID"
        Just el -> return el
