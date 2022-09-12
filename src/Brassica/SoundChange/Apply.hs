module Brassica.SoundChange.Apply
       (
       -- * Sound change application
         applyStatement
       , applyRuleStr
       , applyStatementStr
       , applyChanges
       -- * Logging
       , AppliedRulesTableItem(..)
       , tableItemToHtmlRows
       , applyChangesWithLogs
       , applyChangesWithChanges
       ) where

import Brassica.SoundChange.Apply.Internal
