{-# LANGUAGE ViewPatterns #-}

module Main where

import System.Environment (getArgs)
import System.IO

import qualified Data.ByteString as B
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import SoundChange
import SoundChange.Parse


main :: IO ()
main = do
    [inputFile, wordsFile, outFile] <- getArgs
    inputText <- readFile inputFile

    let (catsText, unlines -> rulesText) = span ('=' `elem`) $ lines inputText
        cats = parseCategoriesSpec catsText

    wordsText <- (unpack . decodeUtf8) <$> B.readFile wordsFile

    case parseRules cats rulesText of
        Left err ->
            putStrLn $ errorBundlePretty err
        Right rules -> do
            let outWordsText = tokeniseAnd applyRules cats rules wordsText
            B.writeFile outFile $ encodeUtf8 $ pack $ detokeniseWords outWordsText
