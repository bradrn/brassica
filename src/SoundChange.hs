module SoundChange where

import SoundChange.Apply
import SoundChange.Category
import SoundChange.Parse
import SoundChange.Types

tokeniseAndApplyRules :: Categories Grapheme -> [Rule] -> String -> String
tokeniseAndApplyRules cats rs = concat . applyRules rs . tokeniseWord (values cats)

applyRules :: [Rule] -> [Grapheme] -> [Grapheme]
applyRules [] = id
applyRules (r:rs) = applyRules rs . applyStr r
