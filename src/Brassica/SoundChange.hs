-- |
-- Module      : Brassica.SoundChange
-- Copyright   : See LICENSE file
-- License     : BSD3
-- Maintainer  : Brad Neimann
--
-- The modules below provide Brassica’s support for sound changes.
-- For further details on their syntax and processing, refer to the
-- [reference guide](https://github.com/bradrn/brassica/blob/v1.0.0/docs/Reference.md),
-- and the documentation of individual modules.
--
-- In brief, a sound changes file passes through the following phases:
--
--     1. First it is /parsed/ to a @'SoundChanges' 'CategorySpec' 'Directive'@
--     2. Next it undergoes /expansion/ to give a @'SoundChanges' 'Expanded' 'GraphemeList'@
--     3. Finally it can be /applied/ to a 'PWord' using 'applyChanges' or similar.
--
-- Words may also be extracted from a words file for application using
-- 'tokeniseWords', and the file can be recreated using
-- 'detokeniseWords' or similar. (For an MDF file one can similarly
-- use 'Brassica.SFM.MDF.tokeniseMDF'.)
--
-- For a simple example, the following sample applies a sound change
-- file to a words file (without error-handling):
--
-- @
-- import System.Environment (getArgs)
-- import Brassica.SoundChange
--
-- main :: IO ()
-- main = do
--     [changesFile, wordsFile] <- getArgs
--     changes <- readFile changesFile
--     words <- readFile wordsFile
--
--     let Right changesParsed = 'parseSoundChanges' changes
--         Right changesExpanded = 'expandSoundChanges' changesParsed
--
--         Right wordsTokenised =
--             'withFirstCategoriesDecl' 'tokeniseWords' changesExpanded words
--
--         wordsOutput = fmap ('applyChanges' changesExpanded) <$> wordsTokenised
--
--     putStrLn $ 'detokeniseWords' $
--         concatMap ('splitMultipleResults' "/") wordsOutput
-- @
--
-- If writing sound changes in Haskell, it is suggested to skip
-- parsing and expansion and directly create a value of type
-- @'SoundChanges' 'Expanded' ('Bool', [t'Grapheme'])@. Expansion is
-- of little use when v'Category's and v'Autosegment's can be assigned
-- names in the code itself.
module Brassica.SoundChange (module X) where

-- NB. Haddock seems to list these re-exports in reverse order
import Brassica.SoundChange.Tokenise  as X
import Brassica.SoundChange.Apply     as X
import Brassica.SoundChange.Expand    as X
import Brassica.SoundChange.Parse     as X
import Brassica.SoundChange.Types     as X
