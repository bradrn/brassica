module Brassica.SoundChange.Apply
       (
       -- * Sound change application
         RuleTag(..)
       , applyStatement
       , applyRuleStr
       , applyStatementStr
       , applyChanges
       -- * Logging
       , LogItem(..)
       , AppliedRulesTableItem(..)
       , toTableItem
       , tableItemToHtmlRows
       , applyStatementWithLog
       , applyChangesWithLog
       , applyChangesWithChanges
       ) where

import Brassica.SoundChange.Apply.Internal
