{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import Control.Applicative (liftA2)
import Data.String (IsString(fromString))
import GHCJS.DOM.Element (setOuterHTML, IsElement, setInnerHTML)
import GHCJS.DOM.Types (MonadJSM, liftJSM)
import Reflex.Dom
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (shamlet)
import Text.Lucius (renderCss, lucius)

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Brassica.SoundChange
       ( OutputMode(..)
       , ApplicationOutput(..)
       , parseTokeniseAndApplyRules
       , tableItemToHtmlRows
       )
import Brassica.SoundChange.Parse (errorBundlePretty, parseSoundChanges)
import Brassica.SoundChange.Tokenise (detokeniseWords')
import Brassica.SoundChange.Types

applyRules :: (T.Text, T.Text, OutputMode) -> T.Text
applyRules (changes, ws, mode) =
    case parseSoundChanges (T.unpack changes) of
        Left e -> "<pre>" <> T.pack (errorBundlePretty e) <> "</pre>"
        Right statements ->
            case parseTokeniseAndApplyRules statements (T.unpack ws) mode Nothing of
                ParseError e -> "<pre>" <> T.pack (errorBundlePretty e) <> "</pre>"
                HighlightedWords result ->
                    T.pack $ escape $ detokeniseWords' highlightWord result
                AppliedRulesTable items ->
                    T.pack $ surroundTable $ concatMap (tableItemToHtmlRows plaintext') items
  where
    -- NB. this is all duplicated from the Qt interop code; need to
    -- figure out a better way of structuring this
    highlightWord (s, False) = concat s
    highlightWord (s, True) = "<b>" ++ concat s ++ "</b>"

    plaintext' :: Statement -> String
    plaintext' (RuleS r) = plaintext r
    plaintext' (CategoriesDeclS _) = "categories … end"

    surroundTable :: String -> String
    surroundTable s = "<table>" ++ s ++ "</table>"

    escape :: String -> String
    escape = concatMap $ \case
        '\n' -> "<br/>"
        -- '\t' -> "&#9;"  -- this doesn't seem to do anything - keeping it here in case I eventually figure out how to do tabs in Qt
        c    -> pure c

br :: DomBuilder t m => m ()
br = el "br" blank

labeledEl
    :: DomBuilder t m
    => T.Text
    -> m el
    -> m el
labeledEl lbl mel = elClass "div" "block" $ do
    text $ lbl <> ":"
    br
    mel

-- Based on reflex elDynHtmlAttr' implementation
elDynHtml
    :: ( DomBuilder t m
       , IsElement (RawElement (DomBuilderSpace m))
       , MonadJSM (Performable m)
       , PerformEvent t m
       , PostBuild t m
       )
    => T.Text
    -> Dynamic t T.Text
    -> m (Element EventResult (DomBuilderSpace m) t)
elDynHtml elementTag html = do
    (e, _) <- element elementTag def $ pure ()
    postBuild <- getPostBuild
    performEvent_ $ liftJSM . setInnerHTML (_element_raw e) <$> leftmost
        [ updated html
        , tag (current html) postBuild
        ]
    pure e

elRawHtml
    :: ( DomBuilder t m
       , IsElement (RawElement (DomBuilderSpace m))
       , MonadJSM m
       )
    => T.Text
    -> m (Element EventResult (DomBuilderSpace m) t)
elRawHtml html = do
    (e, _) <- element "br" def $ pure ()
    liftJSM $ setOuterHTML (_element_raw e) html
    pure e

bigTextAreaConf :: (DomSpace m, Reflex t) => TextAreaElementConfig EventResult t m
bigTextAreaConf = def &
    textAreaElementConfig_elementConfig . elementConfig_initialAttributes .~
        Map.fromList
            [ ("cols", "40")
            , ("rows", "50")
            ]

radio :: DomBuilder t m => T.Text -> T.Text -> T.Text -> Bool -> m (Dynamic t Bool)
radio label ident name checked = do
    e <- inputElement $ def
        & inputElementConfig_initialChecked .~ checked
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
            Map.fromList
                [ ("type", "radio")
                , ("id", ident)
                , ("name", name)
                ]
    elAttr "label" ("for" =: ident) $ text label
    pure $ _inputElement_checked e

style :: IsString s => s
style = fromString $ TL.unpack $ renderCss $ ($ undefined) [lucius|
.block {
    display: inline-block;
    vertical-align: top;
    margin: 15px;
}
|]

intro :: T.Text
intro = TL.toStrict $ renderHtml [shamlet|
<h1>Brassica: online version
<p>
    Brassica is an advanced sound change applier with features including multigraphs and phonetic categories. #
    \ This page runs <b>Brassica 0.0.2</b>. #
<p>
    You are currently on the <b>online version</b> of Brassica. #
    \ You may also want to try the standalone desktop interface, which may be downloaded from the <a href="https://github.com/bradrn/brassica">GitHub repository</a>. #
    \ This online version does not support all features of Brassica: #
    \ in particular it does not include opening or saving, syntax highlighting, the paradigm builder, or a live results view. #
    \ It is also somewhat slow. #
    \ However, otherwise it should be fully compatible with the desktop version of Brassica.
<p>
    For more information on how to use Brassica, see the <a href="https://github.com/bradrn/brassica/blob/211b9c3e9ad97ab509d41847610d7c6a37e5d1fc/Documentation.md">official documentation</a>.
|]

main :: IO ()
main = mainWidgetWithCss style $ el "div" $ do
    _ <- elRawHtml intro

    rules <- labeledEl "Rules" $ textAreaElement bigTextAreaConf
    input <- labeledEl "Input lexicon" $ textAreaElement bigTextAreaConf
    (applyBtn, reportBtn, mode) <- elClass "div" "block" $ do
        applyBtn  <- button "Apply"
        reportBtn <- button "Report rules applied"
        mode <- el "fieldset" $ do
            el "legend" $ text "Output highlighting"
            _nohighlight <- radio "No highlighting"       "nohighlight" "highlighting" True
            br
            dlastrun     <- radio "Different to last run" "dlastrun"    "highlighting" False
            br
            dinput       <- radio "Different to input"    "dinput"      "highlighting" False
            pure $ ffor2 dlastrun dinput $ \dl di ->
                if dl then DifferentToLastRun else
                    if di then DifferentToInput else
                        NoHighlight
        pure (applyBtn, reportBtn, mode)

    let applyEvent = leftmost
            [ current mode <@ applyBtn
            , ReportRulesApplied <$ reportBtn
            ]
    outputValue <- holdDyn "" $
        fmap applyRules $
            current (liftA2 (,,)
                (_textAreaElement_value rules)
                (_textAreaElement_value input))
            <@> applyEvent
    
    _output <- labeledEl "Output lexicon" $
        elDynHtml "p" outputValue
    pure ()
