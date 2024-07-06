{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

{-| This module implements basic support for the SIL Standard Format
Marker (SFM) format, used by dictionary software such as
[FieldWorks](https://software.sil.org/fieldworks/). This format forms
the basis of standards such as Multi-Dictionary Formatter (MDF),
implemented here in 'Brassica.SFM.MDF'.
-}
module Brassica.SFM.SFM
       ( -- * Linear SFM documents
         Field(..)
       , SFM
       , parseSFM
       , exactPrintField
       , exactPrintSFM
       , stripSourcePos
         -- * Hierarchies
       , Hierarchy
       , SFMTree(..)
       , toTree
       , fromTree
       , mapField
       , searchField
       ) where

import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.Map as M

-- | A single field of an SFM file.
data Field = Field
    { fieldMarker :: String
    -- ^ The field marker, ommitting the initial backslash
    , fieldWhitespace :: String
    -- ^ Whitespace after the field marker
    , fieldSourcePos :: Maybe SourcePos
    -- ^ Optionally, a Megaparsec 'SourcePos' marking the start of the value
    , fieldValue :: String
    -- ^ The value of the field, including all whitespace until the next marker
    } deriving (Show)

-- | An SFM file, being a list of fields.
type SFM = [Field]

-- | Set the 'fieldSourcePos' of a 'Field' to 'Nothing'. Useful for
-- making debug output shorter.
stripSourcePos :: Field -> Field
stripSourcePos f = f { fieldSourcePos = Nothing }

type Parser = Parsec Void String

sc :: Parser String
sc = fmap (fromMaybe "") $ optional $
    takeWhile1P (Just "white space") ((&&) <$> isSpace <*> (/='\n'))

-- | Parse until the next backslash at the beginning of a line.
parseFieldValue :: Parser String
parseFieldValue = do
    val <- takeWhileP Nothing (/= '\n')
    observing (char '\n') >>= \case
        Left _ -> val <$ eof
        Right _ -> do
            let val' = val++"\n"
            -- parse more lines if no following backslash
            (notFollowedBy (char '\\') *> ((val'++) <$> parseFieldValue))
                <|> pure val'

entry :: Parser (String, String, SourcePos, String)
entry = do
    _ <- char '\\'
    marker <- takeWhile1P (Just "field name") (not . isSpace)
    s <- sc
    ps <- getSourcePos
    value <- parseFieldValue
    pure (marker, s, ps, value)

-- | Parse an SFM file to an 'SFM'.
parseSFM
    :: String  -- ^ Name of source file
    -> String  -- ^ Input SFM data to parse
    -> Either (ParseErrorBundle String Void) SFM
parseSFM = runParser (sc *> many (toField <$> entry) <* eof)
  where
    toField (f, s, p, v) = Field f s (Just p) v

-- | Print a single field as 'String'.
exactPrintField :: Field -> String
exactPrintField f = '\\' : fieldMarker f ++ fieldWhitespace f ++ fieldValue f

-- | Given an 'SFM', reconstruct the original file. A trivial wrapper
-- around 'exactPrintField'.
exactPrintSFM :: SFM -> String
exactPrintSFM = concatMap exactPrintField

-- | Rose tree describing a hierarchical SFM document.
data SFMTree
    = Root [SFMTree]             -- ^ Root node
    | Filled Field [SFMTree]     -- ^ A 'Field' with zero or more children.
    | Missing String [SFMTree]
    -- ^ A missing level of the hierarchy: a marker which is inferred
    -- from the presence of its children.
    deriving (Show)

-- | The hierarchy underlying an SFM document, defined as a map from
-- field names to their parents. Unlisted fields are treated as roots.
type Hierarchy = M.Map String String

-- | Returns the full hierarchy of a marker, starting with its
-- immediate parent and finishing with the root.
hierarchyFor :: Hierarchy -> String -> [String]
hierarchyFor h = go
  where
    go :: String -> [String]
    go m = case M.lookup m h of
        Just m' -> m' : go m'
        Nothing -> []

(<+:>) :: SFMTree -> SFMTree -> SFMTree
(Root s) <+:> t = Root (s ++ [t])
(Filled f s) <+:> t = Filled f (s ++ [t])
(Missing f s) <+:> t = Missing f (s ++ [t])

-- | Use a 'Hierarchy' to generate a tree structure from an 'SFM'
-- document. Fields are converted to 'Filled' nodes, containing as
-- many following nodes as possible, until the next node which is at
-- the same level of the hierarchy or lower. 'Missing' nodes are
-- created for any missing levels of the hierarchy.
toTree :: Hierarchy -> SFM -> SFMTree
toTree h = fst . go (Root [])
  where
    go :: SFMTree -> SFM -> (SFMTree, SFM)
    go t [] = (t, [])
    go s@(Root _) (f:fs) =
        let (subtree, fs') = go (Filled f []) fs
        in go (s <+:> subtree) fs'
    go s (f:fs) =
        let parentMarker = case s of
                Filled (Field{fieldMarker=m}) _ -> m
                Missing m _ -> m
            hierarchy = hierarchyFor h (fieldMarker f)
        in case break (==parentMarker) hierarchy of
            -- this marker is unrelated to parentMarker, need to
            -- rewind back up the tree and try again
            (_, []) -> (s, f:fs)

            -- otherwise this marker belongs somewhere under
            -- parentMarker

            -- if it is an immediate child, add the subtree directly
            ([], _) ->
                let (subtree, fs') = go (Filled f []) fs
                in go (s <+:> subtree) fs'

            -- otherwise, recurse into the hierarchy
            (m:_, _) ->
                let (subtree, fs') = go (Missing m []) (f:fs)
                in go (s <+:> subtree) fs'

-- | Inverse of 'toTree': convert an 'SFMTree' back into a linear
-- 'SFM' document.
fromTree :: SFMTree -> SFM
fromTree (Root s) = concatMap fromTree s
fromTree (Filled f s) = f : concatMap fromTree s
fromTree (Missing _ s) = concatMap fromTree s

-- | Map a function over all the 'Field's in an 'SFMTree'.
mapField :: (Field -> Field) -> SFMTree -> SFMTree
mapField g (Root s) = Root $ mapField g <$> s
mapField g (Filled f s) = Filled (g f) $ mapField g <$> s
mapField g (Missing m s) = Missing m $ mapField g <$> s

-- | Depth-first search for fields under an 'SFMTree'.
searchField :: (Field -> Maybe a) -> SFMTree -> [a]
searchField p (Root ts) = searchField p =<< ts
searchField p (Filled f ts)
    | Just a <- p f = a : (searchField p =<< ts)
    | otherwise     =      searchField p =<< ts
searchField p (Missing _ ts) = searchField p =<< ts
