-- |
-- Module      : Brassica.SoundChange.Apply
-- Copyright   : See LICENSE file
-- License     : BSD3
-- Maintainer  : Brad Neimann
--
-- This module contains functions to apply one or more expanded sound
-- changes to words.
module Brassica.SoundChange.Apply
       (
       -- * Applying single rules
         applyRuleStr
       , applyStatementStr
       -- * Applying multiple sound changes
       , applyChanges
       , applyChangesWithChanges
       , applyChangesWithReports
       , applyChangesWithChangesAndReports
       , applyChangesWithLogs
       -- * Logs
       , PWordLog(..)
       , reportAsText
       , reportAsHtmlRows
       ) where

import Brassica.SoundChange.Apply.Internal
