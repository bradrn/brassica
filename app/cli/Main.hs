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

    let (catsText, rulesText) = span ('=' `elem`) $ lines inputText
        cats = parseCategoriesSpec catsText
        rules = parseRules cats rulesText

    wordsText <- (unpack . decodeUtf8) <$> B.readFile wordsFile

    let outWordsText = unlines $ fmap (tokeniseAndApplyRules cats rules) $ lines wordsText

    B.writeFile outFile $ encodeUtf8 $ pack outWordsText
