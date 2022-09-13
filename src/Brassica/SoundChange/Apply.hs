module Brassica.SoundChange.Apply
       (
       -- * Sound change application
         applyStatement
       , applyRuleStr
       , applyStatementStr
       , applyChanges
       -- * Logging
       , PWordLog(..)
       , reportAsHtmlRows
       , reportAsText
       , applyChangesWithLogs
       , applyChangesWithChanges
       ) where

import Brassica.SoundChange.Apply.Internal
