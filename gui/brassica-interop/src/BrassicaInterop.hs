{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards          #-}

module BrassicaInterop where

import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Foreign.C
import Foreign.C.String

import SoundChange
import SoundChange.Parse
import SoundChange.Types

parseTokeniseAndApplyRules_hs
    :: CString     -- ^ categories
    -> CString     -- ^ rules
    -> CString     -- ^ words
    -> CBool       -- ^ report rules applied?
    -> IO CString  -- ^ output (either wordlist or parse error)
parseTokeniseAndApplyRules_hs catsRaw rulesRaw wsRaw (CBool report) = do
    catsText <- peekCString catsRaw
    rulesText <- peekCString rulesRaw
    wsText <- peekCString wsRaw

    let cats = parseCategoriesSpec $ lines catsText

    case parseRules cats rulesText of
        Left e -> newCString $ errorBundlePretty e
        Right rules -> newCString $
            if report == 1 then
                let result = tokeniseAnd applyRulesWithLog cats rules wsText
                in intercalate "<br>" $ concat (getWords result) <&> \RuleApplied{..} ->
                    "<b>" ++ unWordPart input ++ "</b> &rarr; <b>" ++ unWordPart output ++ "</b> (" ++ plaintext rule ++ ")"
            else
                detokeniseWords $ tokeniseAnd applyRules cats rules wsText
  where
    unWordPart :: [WordPart] -> String
    unWordPart = concatMap $ fromRight ""

foreign export ccall parseTokeniseAndApplyRules_hs :: CString -> CString -> CString -> CBool -> IO CString
