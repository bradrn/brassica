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
       , tableItemToText
       , applyChangesWithLogs
       , applyChangesWithChanges
       ) where

import Brassica.SoundChange.Apply.Internal
