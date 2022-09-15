{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

{-| This module contains types and functions for working with the MDF
  dictionary format, used by programs such as [SIL Toolbox](https://software.sil.org/toolbox/).
  For more on the MDF format, refer to e.g.
  [Coward & Grimes (2000), /Making Dictionaries: A guide to lexicography and the Multi-Dictionary Formatter/](http://downloads.sil.org/legacy/shoebox/MDF_2000.pdf).
-}
module Brassica.MDF
       (
       -- * MDF files
         MDF(..)
       , MDFLanguage(..)
       , fieldLangs
       -- * Parsing
       , parseMDFRaw
       , parseMDFWithTokenisation
       -- ** Re-export
       , errorBundlePretty
       -- * Conversion
       , componentiseMDF
       , componentiseMDFWordsOnly
       , duplicateEtymologies
       ) where

import Control.Category ((>>>))
import Data.Char (isSpace)
import Data.Void (Void)

import qualified Data.Map as M
import Text.Megaparsec
import Text.Megaparsec.Char

import Brassica.SoundChange.Tokenise
import Brassica.SoundChange.Types (Grapheme, PWord)
import Data.Maybe (fromMaybe)

-- | An MDF (Multi-Dictionary Formatter) file, represented as a list
-- of (field marker, whitespace, field value) tuples. The field marker
-- is represented excluding its initial slash; whitespace after the
-- field marker is also stored, allowing the original MDF file to be
-- precisely recovered. Field values should includes all whitespace to
-- the next marker. All field values are stored as 'String's, with the
-- exception of 'Vernacular' fields, which have type @v@.
--
-- For instance, the following MDF file:
--
-- > \lx kapa
-- > \ps n
-- > \ge parent
-- > \se sakapa
-- > \ge father
--
-- Could be stored as:
--
-- > MDF [ ("lx", " ", Right "kapa\n")
-- >     , ("ps", " ", Left "n\n")
-- >     , ("ge", " ", Left "parent\n")
-- >     , ("se", " ", Right "sakapa\n")
-- >     , ("ge", " ", Left "father")
-- >     ]
newtype MDF v = MDF { unMDF :: [(String, String, Either String v)] }
    deriving (Show, Functor)

type Parser = Parsec Void String

sc :: Parser String
sc = fmap (fromMaybe "") $ optional $ takeWhile1P (Just "white space") isSpace

parseToSlash :: Parser String
parseToSlash = takeWhileP (Just "field value") (/= '\\')

entry :: Parser v -> Parser (String, String, Either String v)
entry pv = do
    _ <- char '\\'
    marker <- takeWhile1P (Just "field name") (not . isSpace)
    s <- sc
    value <- case M.lookup marker fieldLangs of
        Just Vernacular -> Right <$> pv
        _ -> Left <$> parseToSlash
    pure (marker, s, value)

-- | Parse an MDF file to an 'MDF', storing the 'Vernacular' fields as 'String's.
parseMDFRaw :: String -> Either (ParseErrorBundle String Void) (MDF String)
parseMDFRaw = runParser (fmap MDF $ sc *> many (entry parseToSlash) <* eof) ""

-- | Parse an MDF file to an 'MDF', parsing the 'Vernacular' fields
-- into 'Component's in the process.
parseMDFWithTokenisation
    :: [Grapheme]
    -> String
    -> Either (ParseErrorBundle String Void) (MDF [Component PWord])
parseMDFWithTokenisation (sortByDescendingLength -> gs) =
    runParser (fmap MDF $ sc *> p <* eof) ""
  where
    p = many $ entry $ componentsParser $ wordParser "\\" gs

-- | Convert an 'MDF' to a list of 'Component's representing the same
-- textual content. Vernacular field values are left as is; everything
-- else is treated as a 'Separator', so that it is not disturbed by
-- operations such as rule application or rendering to text.
componentiseMDF :: MDF [Component a] -> [Component a]
componentiseMDF = unMDF >>> concatMap \case
    (m, s, Left v) -> [Separator ('\\':m ++ s ++ v)]
    (m, s, Right v) -> Separator ('\\':m ++ s) : v

-- | As with 'componentiseMDF', but the resulting 'Component's contain
-- the contents of 'Vernacular' fields only; all else is
-- discarded. The first parameter specifies the 'Separator' to insert
-- after each vernacular field.
componentiseMDFWordsOnly :: MDF [Component a] -> [Component a]
componentiseMDFWordsOnly = unMDF >>> concatMap \case
    (_, _, Right v) -> v
    _ -> []

-- | Add etymological fields to an 'MDF' by duplicating the values in
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
duplicateEtymologies
    :: (v -> String)
    -- ^ Function to convert from vernacular field values to
    -- strings. Can also be used to preprocess the value of the
    -- resulting @\et@ fields, e.g. by prepending @*@ or similar.
    -> MDF v -> MDF v
duplicateEtymologies typeset = MDF . go Nothing Nothing . unMDF
  where
    mkEt word gloss = word' gloss'
      where
        word' = case word of
            Just et -> (("et", " ", Left $ typeset et) :)
            Nothing -> id
        gloss' = case gloss of
            Just eg -> [("eg", " ", Left eg)]
            Nothing -> []

    go word gloss [] = mkEt word gloss
    go word _ (f@("ge", _, Left gloss'):fs)                  -- store gloss field for future etymology
        = f : go word (Just gloss') fs
    go word gloss (f@(m, _, Right word'):fs)                 -- add etymology & store word if word or subentry field reached
        | m == "lx" || m == "se"
        = mkEt word gloss ++ f : go (Just word') Nothing fs
    go word gloss (f@("dt", _, _):fs)                        -- add etymology if date (usually final field in entry) reached
        = mkEt word gloss ++ f : go Nothing Nothing fs
    go word gloss (f:fs) = f : go word gloss fs
    

-- | The designated language of an MDF field.
data MDFLanguage = English | National | Regional | Vernacular | Other
    deriving (Eq, Show)

-- | A 'M.Map' from the most common field markers to the language of
-- their values.
--
-- (Note: This is currently hardcoded in the source code, based on the
-- values in the MDF definitions from SIL Toolbox. There’s probably a
-- more principled way of defining this, but hardcoding should suffice
-- for now.)
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
