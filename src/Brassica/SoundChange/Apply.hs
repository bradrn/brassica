module Brassica.SoundChange.Apply
       (
       -- * Sound change application
         applyRuleStr
       , applyStatementStr
       , applyChanges
       -- * Logging
       , applyChangesWithLogs
       , applyChangesWithChanges
       , PWordLog(..)
       , reportAsText
       , reportAsHtmlRows
       ) where

import Brassica.SoundChange.Apply.Internal
