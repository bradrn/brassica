{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Reflex.Dom hiding (checkbox)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (shamlet)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Brassica.Paradigm (applyParadigm, parseParadigm, errorBundlePretty)
import Common
    
intro :: T.Text
intro = TL.toStrict $ renderHtml [shamlet|
<h1>Brassica paradigm builder: online version
<p>
    Brassica is an advanced sound change applier with features including multigraphs and phonetic categories. #
    \ This page runs the paradigm builder for the <b>online version</b> of <b>Brassica 0.0.3</b>. #
    \ You may also want to try the standalone desktop interface, which may be downloaded from the <a href="https://github.com/bradrn/brassica">GitHub repository</a>. #
<p>
    For more information on how to use Brassica, see the <a href="https://github.com/bradrn/brassica/blob/211b9c3e9ad97ab509d41847610d7c6a37e5d1fc/Documentation.md">official documentation</a>.
|]

buildParadigm :: T.Text -> T.Text -> T.Text
buildParadigm pText ws =
    case parseParadigm (T.unpack pText) of
        Left e -> "<pre>" <> T.pack (errorBundlePretty e) <> "</pre>"
        Right p -> T.intercalate "<br>" $ fmap T.pack $ concatMap (applyParadigm p) $ lines $ T.unpack ws

main :: IO ()
main = mainWidgetWithCss style $ el "div" $ do
    _ <- elRawHtml intro

    paradigm <- labeledEl "Paradigm" $ textAreaElement bigTextAreaConf
    roots <- labeledEl "Roots" $ textAreaElement bigTextAreaConf
    buildBtn <- elClass "div" "block" $ button "Build"

    let output = buildParadigm <$> _textAreaElement_value paradigm <*> _textAreaElement_value roots
    outputValue <- holdDyn "" $ current output <@ buildBtn

    _output <- labeledEl "Output" $
        elDynHtml "p" outputValue
    pure ()
