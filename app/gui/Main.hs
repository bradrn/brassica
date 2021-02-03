{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Environment (getArgs)
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))

import Data.Map.Strict (keys)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Foreign.JavaScript (IsHandler, JSObject)

import SoundChange.Apply (applyStr)
import SoundChange.Category (values)
import SoundChange.Parse
import SoundChange.Types (Grapheme, Rule)

main :: IO ()
main = do
    setLocaleEncoding utf8
    hSetBuffering stdout LineBuffering
    [port] <- getArgs
    startGUI defaultConfig {
          jsCustomHTML = Just "gui.html"
        , jsStatic = Just "static"
        , jsPort = Just (read port)
        } setup

setup :: Window -> UI ()
setup window = do
    cats     <- getElementById' "categories"
    rules    <- getElementById' "rules"
    words    <- getElementById' "words"
    applyBtn <- getElementById' "applyBtn"
    out      <- getElementById' "out"

    -- A 'Behaviour' to keep track of the previous output.
    (prevOutE, setPrevOut) <- liftIO $ newEvent @[[[Grapheme]]]
    prevOut <- stepper [] prevOutE

    on UI.keydown cats $ const $ do
        catsText  <- lines <$> get value cats
        rehighlight catsText

    on UI.click applyBtn $ const $ do
        catsText  <- lines <$> get value cats
        rulesText <- fmap lines $ callFunction $ ffi "rulesCodeMirror.getValue()"
        wordsText <- lines <$> get value words

        let cats = parseCategoriesSpec catsText
            rules = parseRules cats rulesText

            results = fmap (applyRulesWithChanges rules) . tokeniseWords (values cats) <$> wordsText

        prevResults <- liftIO $ currentValue prevOut
        liftIO $ setPrevOut $ (fmap.fmap) fst results

        results' <- getHlMode <&> \mode -> fmap unwords $
            zipWith2' results prevResults [] $ \(word, hasBeenAltered) prevWord ->
                case mode of
                    HlNone -> concat word
                    HlRun -> surroundBold (word /= prevWord) $ concat word
                    HlInput -> surroundBold hasBeenAltered $ concat word

        element out # set html (unlines results')

    _ <- exportAs "openRules" $ runUI window . openRules cats rules
    _ <- exportAs "saveRules" $ runUI window . saveRules cats rules
    _ <- exportAs "openLexicon" $ runUI window . openLexicon words
    _ <- exportAs "saveLexicon" $ runUI window . saveLexicon words

    return ()
  where
    getElementById' i = getElementById window i >>= \case
        Nothing -> error "Tried to get nonexistent ID"
        Just el -> return el

    surroundBold False w = w
    surroundBold True  w = "<b>" ++ w ++ "</b>"

    zipWith2' :: [[a]] -> [[b]] -> b -> (a -> b -> c) -> [[c]]
    zipWith2' ass bss bd f = zipWith' ass bss [] $ \as bs -> zipWith' as bs bd f

    zipWith' :: [a] -> [b] -> b -> (a -> b -> c) -> [c]
    zipWith' []      _     _ _ = []
    zipWith' as     []     bd f = fmap (`f` bd) as
    zipWith' (a:as) (b:bs) bd f = f a b : zipWith' as bs bd f

applyRulesWithChanges :: [Rule] -> [Grapheme] -> ([Grapheme], Bool)
applyRulesWithChanges = flip (go . (,False))
  where
    go gs [] = gs
    go (gs,w) (r:rs) =
        let gs' = applyStr r gs
        in go (gs', w || (gs/=gs')) rs

data HlMode = HlNone | HlRun | HlInput deriving (Show)

getHlMode :: UI HlMode
getHlMode = callFunction (ffi @(JSFunction String) "$('input[name=hl-options]:checked').val()") >>= \case
    "hl-none"  -> return HlNone
    "hl-run"   -> return HlRun
    "hl-input" -> return HlInput
    _ -> error "unexpected highlighting mode"

rehighlight :: [String] -> UI ()
rehighlight catsText =
    let catNames = keys $ parseCategoriesSpec catsText
    in runFunction $ ffi "setupMode(%1)" catNames

exportAs :: IsHandler a => String -> a -> UI JSObject
exportAs name h = do
    ref <- ffiExport h
    runFunction $ ffi ("window.hs." ++ name ++ " = %1") ref
    return ref

openRules :: Element -> Element -> FilePath -> UI ()
openRules cats rules path = do
    (catsText, rulesText) <- liftIO $ decodeRules <$> readFile path
    runFunction $ ffi "rulesCodeMirror.setValue(%1)" rulesText
    _ <- element cats # set value catsText
    rehighlight $ lines catsText

    return ()

decodeRules :: String -> (String, String)
decodeRules = bimap unlines (unlines.tail) . span (/="[rules]") . lines

saveRules :: Element -> Element -> FilePath -> UI ()
saveRules cats rules path = do
    catsText <- get value cats
    rulesText <- callFunction $ ffi "rulesCodeMirror.getValue()"

    liftIO $ writeFile path $ encodeRules catsText rulesText

encodeRules
    :: String  -- ^ Categories
    -> String  -- ^ Rules
    -> String
encodeRules cats rules = cats ++ "\n[rules]\n" ++ rules

openLexicon :: Element -> FilePath -> UI ()
openLexicon words path = liftIO (readFile path) >>= ($ element words) . set value >> pure ()

saveLexicon :: Element -> FilePath -> UI ()
saveLexicon words path = get value words >>= liftIO . writeFile path
