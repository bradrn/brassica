-- |
-- Module      : Brassica.SoundChange.Apply
-- Copyright   : See LICENSE file
-- License     : BSD3
-- Maintainer  : Brad Neimann
--
-- This module contains functions to apply one or more expanded sound
-- changes to words.
--
-- The most important function is 'applyChanges', which applies a set
-- of 'Brassica.SoudnChanges.Types.SoundChanges' to an input word. It
-- returns a log of all sound changes and other actions which were
-- applied to produce intermediate forms and the final result. The
-- results can be summarised using the functions in
-- [Reporting results](#g:3).
module Brassica.SoundChange.Apply
       (
       -- * Applying single rules
         applyRuleStr
       , applyStatementStr
       -- * Applying multiple sound changes
       , applyChanges
       , PWordLog(..)
       , LogItem(..)
       -- * Reporting results
       , getOutput
       , getReports
       , getChangedOutputs
       , getChangedReports
       , reportAsText
       , reportAsHtmlRows
       ) where

import Brassica.SoundChange.Apply.Internal
