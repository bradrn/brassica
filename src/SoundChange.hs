module SoundChange where

import SoundChange.Apply
import SoundChange.Category
import SoundChange.Parse
import SoundChange.Types

tokeniseAndApplyRules :: Categories Grapheme -> [Rule] -> String -> String
tokeniseAndApplyRules cats rs = detokeniseWords . fmap (fmap $ applyRules rs . fmap Right) . tokeniseWords (values cats)

applyRules :: [Rule] -> [WordPart] -> [WordPart]
applyRules [] = id
applyRules (r:rs) = applyRules rs . applyStr r
