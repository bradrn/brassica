{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import Control.Applicative (liftA2)
import Data.FileEmbed (makeRelativeToProject, embedFile)
import Data.Map.Strict (fromList, Map)
import Data.Text.Encoding (decodeUtf8)
import Reflex.Dom hiding (checkbox)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (shamlet)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Common

import Brassica.SoundChange.Frontend.Internal
       ( ApplicationMode(..)
       , HighlightMode(..)
       , MDFOutputMode(MDFOutput)
       , TokenisationMode (Normal)
       , ApplicationOutput(..)
       , InputLexiconFormat(Raw)
       , parseTokeniseAndApplyRules
       )
import Brassica.SoundChange.Apply (reportAsHtmlRows)
import Brassica.SoundChange.Parse (errorBundlePretty, parseSoundChanges)
import Brassica.SoundChange.Tokenise (detokeniseWords', Component)
import Brassica.SoundChange.Types

applyRules
    :: (Maybe [Component PWord], (T.Text, T.Text, ApplicationMode))
    -> (Maybe [Component PWord], T.Text)
applyRules (prev, (changes, ws, mode)) =
    case parseSoundChanges (T.unpack changes) of
        Left e -> (Nothing, "<pre>" <> T.pack (errorBundlePretty e) <> "</pre>")
        Right statements ->
            case parseTokeniseAndApplyRules statements (T.unpack ws) Raw Normal mode prev of
                ParseError e -> (Nothing, "<pre>" <> T.pack (errorBundlePretty e) <> "</pre>")
                HighlightedWords result ->
                    (Just $ (fmap.fmap) fst result, T.pack $ escape $ detokeniseWords' highlightWord result)
                AppliedRulesTable items ->
                    (Nothing, T.pack $ surroundTable $ concatMap (reportAsHtmlRows plaintext') items)
  where
    -- NB. this is all duplicated from the Qt interop code; need to
    -- figure out a better way of structuring this
    highlightWord (s, False) = concat s
    highlightWord (s, True) = "<b>" ++ concat s ++ "</b>"

    surroundTable :: String -> String
    surroundTable s = "<table>" ++ s ++ "</table>"

    escape :: String -> String
    escape = concatMap $ \case
        '\n' -> "<br/>"
        -- '\t' -> "&#9;"  -- this doesn't seem to do anything - keeping it here in case I eventually figure out how to do tabs in Qt
        c    -> pure c

data Example = Example
    { exampleRules :: T.Text
    , exampleWords :: T.Text
    } deriving (Show, Eq, Ord)

blankExample :: Example
blankExample = Example "" ""

examples :: Map Example T.Text
examples = fromList
    [ (blankExample, "None")
    , (mkExample englishRules englishWords        , "Middle to Modern English")
    , (mkExample latin2portRules latin2portWords  , "Latin to Portuguese")
    , (mkExample tonogenesisRules tonogenesisWords, "Tonogenesis")
    ]
  where
    mkExample r w = Example
        { exampleRules = decodeUtf8 r
        , exampleWords = decodeUtf8 w
        }

    englishRules     = $(makeRelativeToProject "../../examples/english.bsc" >>= embedFile)
    latin2portRules  = $(makeRelativeToProject "../../examples/latin2port.bsc" >>= embedFile)
    tonogenesisRules = $(makeRelativeToProject "../../examples/tonogenesis.bsc" >>= embedFile)

    englishWords     = $(makeRelativeToProject "../../examples/english.lex" >>= embedFile)
    latin2portWords  = $(makeRelativeToProject "../../examples/latin2port.lex" >>= embedFile)
    tonogenesisWords = $(makeRelativeToProject "../../examples/tonogenesis.lex" >>= embedFile)

intro :: T.Text
intro = TL.toStrict $ renderHtml [shamlet|
<h1>Brassica: online version
<p>
    Brassica is an advanced sound change applier with features including multigraphs and phonetic categories. #
    \ This page runs <b>Brassica 0.0.3</b>. #
<p>
    You are currently on the <b>online version</b> of Brassica. #
    \ You may also want to try the standalone desktop interface, which may be downloaded from the <a href="https://github.com/bradrn/brassica">GitHub repository</a>. #
    \ This online version does not support all features of Brassica: #
    \ in particular it does not include opening or saving, or syntax highlighting. #
    \ It is also somewhat slow. #
    \ However, otherwise it should be fully compatible with the desktop version of Brassica.
<p>
    For more information on how to use Brassica, see the <a href="https://github.com/bradrn/brassica/blob/v0.0.3/Documentation.md">official documentation</a>.
|]

main :: IO ()
main = mainWidgetWithCss style $ el "div" $ do
    _ <- elRawHtml intro

    rec
        rules <- labeledEl "Rules" $ textAreaElement bigTextAreaConf
            { _textAreaElementConfig_setValue = Just (exampleRules <$> setExample)
            }
        input <- labeledEl "Input lexicon" $ textAreaElement bigTextAreaConf
            { _textAreaElementConfig_setValue = Just (exampleWords <$> setExample)
            }
        (applyBtn, reportBtn, mode, viewLiveBtn, setExample) <- elClass "div" "block" $ do
            applyBtn  <- button "Apply"
            reportBtn <- button "Report rules applied"
            mode <- el "fieldset" $ do
                el "legend" $ text "Output highlighting"
                _nohighlight <- radio "No highlighting"       "nohighlight" "highlighting" True
                br
                dlastrun     <- radio "Different to last run" "dlastrun"    "highlighting" False
                br
                dinput       <- radio "Different to input"    "dinput"      "highlighting" False
                pure $ ffor2 dlastrun dinput $ \dl di -> flip ApplyRules MDFOutput $
                    if dl then DifferentToLastRun else
                        if di then DifferentToInput else
                            NoHighlight
            br
            viewLiveBtn <- checkbox "View results live" "viewlive" False
            br
            elAttr "a" (fromList [("href", "./builder/"), ("target", "_blank")]) $ text "Open paradigm builder"
            br
            let overwriteMsg = "This will overwrite your current rules and lexicon. Are you sure you want to proceed?"
            exampleBtn <- withConfirm overwriteMsg =<< button "Open example:"
            example <- _dropdown_value <$> dropdown blankExample (pure examples) def
            pure (applyBtn, reportBtn, mode, viewLiveBtn, current example <@ exampleBtn)

    let anyInput = _textAreaElement_input rules <> _textAreaElement_input input
        applyEvent = leftmost
            [ current mode <@ applyBtn
            , gate (current viewLiveBtn) $ current mode <@ anyInput
            , ReportRulesApplied <$ reportBtn
            ]
        appliedValuesEvent =
            -- need to do things promptly here since event can come at
            -- same time as input is modified
            attachPromptlyDynWith ($)
                (liftA2 (,,)
                    (_textAreaElement_value rules)
                    (_textAreaElement_value input))
                applyEvent
    rec
        let withPrev = attach (current prev) appliedValuesEvent
            applicationResult = applyRules <$> withPrev
        prev <- holdDyn Nothing $ fst <$> applicationResult
    outputValue <- holdDyn "" $ snd <$> applicationResult
    
    _output <- labeledEl "Output lexicon" $
        elDynHtml "p" outputValue
    pure ()
