-- |
-- Module      : Brassica.Paradigm
-- Copyright   : See LICENSE file
-- License     : BSD3
-- Maintainer  : Brad Neimann
--
module Brassica.Paradigm
       (
       -- * Re-export
         module Text.Megaparsec.Error
       -- * Paradigm representation
       , Process(..)
       , Affix
       , Grammeme(..)
       , AbstractGrammeme(..)
       , Condition(..)
       , Feature(..)
       , FeatureName(..)
       , Statement(..)
       , Paradigm
       -- * Parsing and pretty-printing
       , parseParadigm
       , formatNested
       -- * Paradigm application
       , applyParadigm
       , ResultsTree(..)
       , depth
       ) where

import Brassica.Paradigm.Apply
import Brassica.Paradigm.Parse
import Brassica.Paradigm.Types
import Text.Megaparsec.Error
