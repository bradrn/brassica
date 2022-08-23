{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Common where

import Control.Monad ((<=<))
import Data.String (IsString(fromString))
import GHCJS.DOM.Element (setOuterHTML, IsElement, setInnerHTML)
import GHCJS.DOM.Types (MonadJSM, liftJSM)
import Language.Javascript.JSaddle (valToBool, jsg1)
import Reflex.Dom hiding (checkbox)
import Text.Lucius (renderCss, lucius)

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

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

checkbox :: DomBuilder t m => T.Text -> T.Text -> Bool -> m (Dynamic t Bool)
checkbox label ident checked = do
    e <- inputElement $ def
        & inputElementConfig_initialChecked .~ checked
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
            Map.fromList
                [ ("type", "checkbox")
                , ("id", ident)
                ]
    elAttr "label" ("for" =: ident) $ text label
    pure $ _inputElement_checked e

confirm :: MonadJSM m => T.Text -> m Bool
confirm = liftJSM . (valToBool <=< jsg1 ("confirm" :: T.Text))

withConfirm
    :: ( MonadJSM (Performable m)
       , PerformEvent t m
       )
    => T.Text -> Event t a -> m (Event t a)
withConfirm msg = fmap (fmapMaybe id) . performEvent . fmap (\a -> tag' a <$> confirm msg)
  where
    tag' _ False = Nothing
    tag' a True = Just a

style :: IsString s => s
style = fromString $ TL.unpack $ renderCss $ ($ undefined) [lucius|
.block {
    display: inline-block;
    vertical-align: top;
    margin: 15px;
}
|]
