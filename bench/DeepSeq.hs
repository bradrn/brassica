{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE GADTs #-}
module DeepSeq where

import Brassica.SoundChange.Types
import Control.DeepSeq
import GHC.Generics

deriving instance Generic Statement
deriving instance Generic Rule
deriving instance Generic CategoriesDecl
deriving instance Generic Flags
deriving instance Generic Direction

deriving instance NFData Statement
deriving instance NFData Rule
deriving instance NFData CategoriesDecl
deriving instance NFData Flags
deriving instance NFData Direction

instance NFData (Lexeme a) where
    rnf (Grapheme a) = rnf a
    rnf (Category a) = rnf a
    rnf (Optional a) = rnf a
    rnf (Wildcard a) = rnf a
    rnf (Kleene a) = rnf a
    rnf Boundary = ()
    rnf Metathesis = ()
    rnf Geminate = ()
    rnf Discard = ()


instance NFData (CategoryElement a) where
    rnf BoundaryEl = ()
    rnf (GraphemeEl a) = rnf a
