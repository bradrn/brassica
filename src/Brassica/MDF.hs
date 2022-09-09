{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Brassica.MDF
       ( MDF
       , MDFLanguage(..)
       , fieldLangs
       , parseMDFRaw
       , parseMDFWithTokenisation
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
-- is represented excluding its initial slash; the field value
-- includes all whitespace to the next marker. 'Vernacular' fields
-- have value type @v@, while all other fields are given 'String'
-- values. Whitespace after the field marker is included so that the
-- original MDF file can be precisely recovered, though it has to be a
-- separate field as looking up field markers could be difficult
-- otherwise.
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

parseMDFRaw :: String -> Either (ParseErrorBundle String Void) (MDF String)
parseMDFRaw = runParser (fmap MDF $ sc *> many (entry parseToSlash) <* eof) ""

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
-- else is wrapped in 'Whitespace' (even though it contains words) so
-- that it is not disturbed by rule application or rendering to text.
componentiseMDF :: MDF [Component a] -> [Component a]
componentiseMDF = unMDF >>> concatMap \case
    (m, s, Left v) -> [Whitespace ('\\':m ++ s ++ v)]
    (m, s, Right v) -> Whitespace ('\\':m ++ s) : v

-- | As with 'MDF', but the resulting 'Component's contain vernacular
-- field contents only; all else is discarded. The first parameter
-- specifies the 'Whitespace' separator to insert after each
-- vernacular field.
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
