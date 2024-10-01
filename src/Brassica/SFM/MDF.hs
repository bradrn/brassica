{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Module      : Brassica.SFM.MDF
-- Copyright   : See LICENSE file
-- License     : BSD3
-- Maintainer  : Brad Neimann
--
-- This module contains types and functions for working with the MDF
-- dictionary format. For more on the MDF format, refer to e.g.
-- [Coward & Grimes (2000)](http://downloads.sil.org/legacy/shoebox/MDF_2000.pdf).
module Brassica.SFM.MDF where

import Brassica.SFM.SFM

import qualified Data.Map as M
import Brassica.SoundChange.Tokenise
import Brassica.SoundChange.Types (PWord)
import Text.Megaparsec (State(..), PosState (..), ParseErrorBundle, runParser')
import Text.Megaparsec.State (initialPosState)
import Data.Void (Void)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

-- | The designated language of an MDF field.
data MDFLanguage = English | National | Regional | Vernacular | Other
    deriving (Eq, Show)

-- | A 'M.Map' from the most common field markers to the language of
-- their values.
--
-- (Note: This is currently hardcoded in the source code, based on the
-- values in the MDF definitions from SIL Toolbox. The exception is
-- @\et@, which is assigned as 'Other' rather than
-- 'Vernacular'. There’s probably a more principled way of defining
-- this, but hardcoding should suffice for now.)
fieldLangs :: M.Map String MDFLanguage
fieldLangs = M.fromList
    [ ("1d" , Vernacular) , ("1e" , Vernacular) , ("1i" , Vernacular)
    , ("1p" , Vernacular) , ("1s" , Vernacular) , ("2d" , Vernacular)
    , ("2p" , Vernacular) , ("2s" , Vernacular) , ("3d" , Vernacular)
    , ("3p" , Vernacular) , ("3s" , Vernacular) , ("4d" , Vernacular)
    , ("4p" , Vernacular) , ("4s" , Vernacular) , ("a"  , Vernacular)
    , ("an" , Vernacular) , ("bb" , English)    , ("bw" , English)
    , ("ce" , English)    , ("cf" , Vernacular) , ("cn" , National)
    , ("cr" , National)   , ("de" , English)    , ("dn" , National)
    , ("dr" , Regional)   , ("dt" , Other)      , ("dv" , Vernacular)
    , ("ec" , English)    , ("ee" , English)    , ("eg" , English)
    , ("en" , National)   , ("er" , Regional)   , ("es" , English)
    , ("et" , Other)  {- defined as vernacular in SIL Toolbox, but by
                         definition it's really a different language -}
    , ("ev" , Vernacular) , ("ge" , English)
    , ("gn" , National)   , ("gr" , Regional)   , ("gv" , Vernacular)
    , ("hm" , English)    , ("is" , English)    , ("lc" , Vernacular)
    , ("le" , English)    , ("lf" , English)    , ("ln" , National)
    , ("lr" , Regional)   , ("lt" , English)    , ("lv" , Vernacular)
    , ("lx" , Vernacular) , ("mn" , Vernacular) , ("mr" , Vernacular)
    , ("na" , English)    , ("nd" , English)    , ("ng" , English)
    , ("np" , English)    , ("nq" , English)    , ("ns" , English)
    , ("nt" , English)    , ("oe" , English)    , ("on" , National)
    , ("or" , Regional)   , ("ov" , Vernacular) , ("pc" , English)
    , ("pd" , English)    , ("pde", English)    , ("pdl", English)
    , ("pdn", National)   , ("pdr", Regional)   , ("pdv", Vernacular)
    , ("ph" , Other)      , ("pl" , Vernacular) , ("pn" , National)
    , ("ps" , English)    , ("rd" , Vernacular) , ("re" , English)
    , ("rf" , English)    , ("rn" , National)   , ("rr" , Regional)
    , ("sc" , English)    , ("sd" , English)    , ("se" , Vernacular)
    , ("sg" , Vernacular) , ("sn" , English)    , ("so" , English)
    , ("st" , English)    , ("sy" , Vernacular) , ("tb" , English)
    , ("th" , Vernacular) , ("u"  , Vernacular) , ("ue" , English)
    , ("un" , National)   , ("ur" , Regional)   , ("uv" , Vernacular)
    , ("va" , Vernacular) , ("ve" , English)    , ("vn" , National)
    , ("vr" , Regional)   , ("we" , English)    , ("wn" , National)
    , ("wr" , Regional)   , ("xe" , English)    , ("xn" , National)
    , ("xr" , Regional)   , ("xv" , Vernacular)
    ]

-- | Standard MDF hierarchy, with @\lx@ > @\se@ > @\ps@ > @\sn@.
-- Intended for use with 'toTree'.
mdfHierarchy :: Hierarchy
mdfHierarchy = M.fromList
    [ ("1d", "ps"), ("1e", "ps"), ("1i", "ps"), ("1p", "ps"), ("1s", "ps")
    , ("2d", "ps"), ("2p", "ps"), ("2s", "ps"), ("3d", "ps"), ("3p", "ps")
    , ("3s", "ps"), ("4d", "ps"), ("4p", "ps"), ("4s", "ps"), ("a", "lx")
    , ("an", "sn"), ("bb", "sn"), ("bw", "se"), ("ce", "cf"), ("cf", "sn")
    , ("cn", "cf"), ("cr", "cf"), ("de", "sn"), ("dn", "sn"), ("dr", "sn")
    , ("dt", "lx"), ("dv", "sn"), ("ec", "et"), ("ee", "sn"), ("eg", "et")
    , ("en", "sn"), ("er", "sn"), ("es", "et"), ("et", "se"), ("ev", "sn")
    , ("ge", "sn"), ("gn", "sn"), ("gr", "sn"), ("gv", "sn"), ("hm", "lx")
    , ("is", "sn"), ("lc", "lx"), ("le", "lv"), ("lf", "sn"), ("ln", "lv")
    , ("lr", "lv"), ("lt", "sn"), ("lv", "lf"), ("mn", "se"), ("mr", "se")
    , ("na", "sn"), ("nd", "sn"), ("ng", "sn"), ("np", "sn"), ("nq", "sn")
    , ("ns", "sn"), ("nt", "sn"), ("oe", "sn"), ("on", "sn"), ("or", "sn")
    , ("ov", "sn"), ("pc", "sn"), ("pd", "ps"), ("pde", "pdl")
    , ("pdl", "pd"), ("pdn", "pdl"), ("pdr", "pdl"), ("pdv", "pdl")
    , ("ph", "se"), ("pl", "ps"), ("pn", "ps"), ("ps", "se"), ("rd", "ps")
    , ("re", "sn"), ("rf", "sn"), ("rn", "sn"), ("rr", "sn"), ("sc", "sn")
    , ("sd", "sn"), ("se", "lx"), ("sg", "ps"), ("sn", "ps"), ("so", "sn")
    , ("st", "lx"), ("sy", "sn"), ("tb", "sn"), ("th", "sn"), ("u", "lx")
    , ("ue", "sn"), ("un", "sn"), ("ur", "sn"), ("uv", "sn"), ("va", "sn")
    , ("ve", "va"), ("vn", "va"), ("vr", "va"), ("we", "sn"), ("wn", "sn")
    , ("wr", "sn"), ("xe", "xv"), ("xn", "xv"), ("xr", "xv"), ("xv", "rf")
    ]

-- | Alternate MDF hierarchy, with @\lx@ > @\sn@ > @\se@ > @\ps@.
-- Intended for use with 'toTree'.
mdfAlternateHierarchy :: Hierarchy
mdfAlternateHierarchy = M.fromList
    [ ("1d", "ps"), ("1e", "ps"), ("1i", "ps"), ("1p", "ps"), ("1s", "ps")
    , ("2d", "ps"), ("2p", "ps"), ("2s", "ps"), ("3d", "ps"), ("3p", "ps")
    , ("3s", "ps"), ("4d", "ps"), ("4p", "ps"), ("4s", "ps")
    , ("an", "ps"), ("bb", "ps"), ("bw", "se"), ("ce", "cf"), ("cf", "ps")
    , ("cn", "cf"), ("cr", "cf"), ("de", "ps"), ("dn", "ps"), ("dr", "ps")
    , ("dt", "lx"), ("dv", "ps"), ("ec", "et"), ("ee", "ps"), ("eg", "et")
    , ("en", "ps"), ("er", "ps"), ("es", "et"), ("et", "se"), ("ev", "ps")
    , ("ge", "ps"), ("gn", "ps"), ("gr", "ps"), ("gv", "ps"), ("hm", "lx")
    , ("is", "ps"), ("lc", "lx"), ("le", "lv"), ("lf", "ps"), ("ln", "lv")
    , ("lr", "lv"), ("lt", "ps"), ("lv", "lf"), ("mn", "se"), ("mr", "se")
    , ("na", "ps"), ("nd", "ps"), ("ng", "ps"), ("np", "ps"), ("nq", "ps")
    , ("ns", "ps"), ("nt", "ps"), ("oe", "ps"), ("on", "ps"), ("or", "ps")
    , ("ov", "ps"), ("pc", "ps"), ("pd", "ps"), ("pde", "pdl")
    , ("pdl", "pd"), ("pdn", "pdl"), ("pdr", "pdl"), ("pdv", "pdl")
    , ("ph", "se"), ("pl", "ps"), ("pn", "ps"), ("ps", "se"), ("rd", "ps")
    , ("re", "ps"), ("rf", "ps"), ("rn", "ps"), ("rr", "ps"), ("sc", "ps")
    , ("sd", "ps"), ("se", "sn"), ("sg", "ps"), ("sn", "lx"), ("so", "ps")
    , ("st", "lx"), ("sy", "ps"), ("tb", "ps"), ("th", "ps")
    , ("ue", "ps"), ("un", "ps"), ("ur", "ps"), ("uv", "ps"), ("va", "se")
    , ("ve", "va"), ("vn", "va"), ("vr", "va"), ("we", "ps"), ("wn", "ps")
    , ("wr", "ps"), ("xe", "xv"), ("xn", "xv"), ("xr", "xv"), ("xv", "rf")
    ]

-- | Convert an 'SFM' document to a list of 'Component's representing
-- the same textual content. 'Vernacular' field values are tokenised as
-- if using 'tokeniseWords'; everything else is treated as a
-- 'Separator', so that it is not disturbed by operations such as rule
-- application or rendering to text.
--
-- (This is a simple wrapper around 'tokeniseField'.)
tokeniseMDF
    :: [String]  -- ^ List of available multigraphs (as with 'tokeniseWord')
    -> SFM -> Either (ParseErrorBundle String Void) [Component PWord]
tokeniseMDF gs = fmap concat . traverse (tokeniseField gs)

-- | Like 'tokeniseMDF', but for a single 'Field' rather than a whole
-- SFM file.
tokeniseField :: [String] -> Field -> Either (ParseErrorBundle String Void) [Component PWord]
tokeniseField gs f = case M.lookup (fieldMarker f) fieldLangs of
    Just Vernacular ->
        -- initialise megaparsec state with position starting at given
        -- field
        let ps = initialPosState "" (fieldValue f)
            s = State
                { stateInput = fieldValue f
                , stateOffset = 0
                , statePosState = case fieldSourcePos f of
                    Nothing -> ps
                    Just sp -> ps { pstateSourcePos = sp }
                , stateParseErrors = []
                }
        in case runParser' (componentsParser $ wordParser "[" gs) s of
            (_, Right cs) -> Right $ Separator ('\\' : fieldMarker f ++ fieldWhitespace f) : cs
            (_, Left err) -> Left err

    _ -> Right [Separator $ '\\' : fieldMarker f ++ fieldWhitespace f ++ fieldValue f]

-- | Add etymological fields to an MDF file by duplicating the values in
-- @\lx@, @\se@ and @\ge@ fields. e.g.:
--
-- > \lx kapa
-- > \ps n
-- > \ge parent
-- > \se sakapa
-- > \ge father
--
-- Would become:
--
-- > \lx kapa
-- > \ps n
-- > \ge parent
-- > \et kapa
-- > \eg parent
-- > \se sakapa
-- > \ge father
-- > \et sakapa
-- > \eg father
--
-- This can be helpful when applying sound changes to an MDF file: the
-- vernacular words can be copied as etymologies, and then the sound
-- changes can be applied leaving the etymologies as is.
--
-- Note that the hierarchy must already be resolved before this
-- function can be used, as it depends on the tree structure to know
-- where the etymologies should be placed.
duplicateEtymologies
    :: (String -> String)
    -- ^ Transformation to apply to etymologies, e.g. @('*':)@
    -> SFMTree
    -> SFMTree
duplicateEtymologies f = go Nothing Nothing
  where
    -- strategy: find each \se (implicit or explicit) with its \ge
    -- and make an \et under it
    go lx gl (Root ts) = Root $ go lx gl <$> ts
    go _lx gl t@(Filled m@(Field { fieldMarker="lx", fieldValue }) ts) =
        let lx = Just fieldValue
            gl' = case searchField isGloss t of
                gl'':_ -> Just gl''
                _ -> gl
        in Filled m $ go lx gl' <$> ts
    go _lx gl t@(Filled m@(Field { fieldMarker="se", fieldValue }) ts) =
        let lx = Just fieldValue
            gl' = case searchField isGloss t of
                gl'':_ -> Just gl''
                _ -> gl
        in Filled m $ ts ++ mkEt lx gl'
    go lx gl (Filled m ts) = Filled m $ go lx gl <$> ts
    go lx gl (Missing "se" ts) = Missing "se" $ ts ++ mkEt lx gl
    go lx gl (Missing m ts) = Missing m $ go lx gl <$> ts

    isGloss Field{fieldMarker,fieldValue}
        | fieldMarker == "ge" = Just fieldValue
        | otherwise = Nothing

    mkEt :: Maybe String -> Maybe String -> [SFMTree]
    mkEt Nothing _ = []  -- can't make etymology without lexeme
    mkEt (Just lx) gl = pure $
        Filled Field
            { fieldMarker = "et"
            , fieldWhitespace = " "
            , fieldSourcePos = Nothing
            , fieldValue = ensureNewline $ f $ trim lx
            }
        $ case gl of
              Nothing -> []
              Just gl' ->
                  [ Filled Field
                      { fieldMarker = "eg"
                      , fieldWhitespace = " "
                      , fieldSourcePos = Nothing
                      , fieldValue = ensureNewline gl'
                      } []
                  ]

    trim = dropWhile isSpace . dropWhileEnd isSpace

    ensureNewline s
        | last s == '\n' = s
        | otherwise = s ++ "\n"
