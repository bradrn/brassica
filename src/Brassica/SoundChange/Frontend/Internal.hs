{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TupleSections   #-}

{-| __Warning:__ This module is __internal__, and does __not__ follow
  the Package Versioning Policy. It may be useful for extending
  Brassica, but be prepared to track development closely if you import
  this module.
-}
module Brassica.SoundChange.Frontend.Internal where

import Control.Monad ((<=<))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Void (Void)
import GHC.Generics (Generic)

import Control.DeepSeq (NFData)
import Text.Megaparsec (ParseErrorBundle)

import Brassica.SFM.MDF
import Brassica.SFM.SFM
import Brassica.SoundChange.Apply
import Brassica.SoundChange.Apply.Internal (applyChangesWithLog, toPWordLog)
import Brassica.SoundChange.Tokenise
import Brassica.SoundChange.Types
import Data.Bifunctor (first)

-- | Rule application mode of the SCA.
data ApplicationMode
    = ApplyRules HighlightMode OutputMode String
    | ReportRulesApplied
    deriving (Show, Eq)

getOutputMode :: ApplicationMode -> OutputMode
getOutputMode (ApplyRules _ o _) = o
getOutputMode ReportRulesApplied = WordsOnlyOutput  -- default option

data HighlightMode
    = NoHighlight
    | DifferentToLastRun
    | DifferentToInput
    deriving (Show, Eq)
instance Enum HighlightMode where
    -- used for conversion to and from C, so want control over values
    fromEnum NoHighlight = 0
    fromEnum DifferentToLastRun = 1
    fromEnum DifferentToInput = 2

    toEnum 0 = NoHighlight
    toEnum 1 = DifferentToLastRun
    toEnum 2 = DifferentToInput
    toEnum _ = undefined

data OutputMode
    = MDFOutput
    | WordsOnlyOutput
    | MDFOutputWithEtymons
    | WordsWithProtoOutput
    deriving (Show, Eq)
instance Enum OutputMode where
    -- used for conversion to and from C, so want control over values
    fromEnum MDFOutput = 0
    fromEnum WordsOnlyOutput = 1
    fromEnum MDFOutputWithEtymons = 2
    fromEnum WordsWithProtoOutput = 3

    toEnum 0 = MDFOutput
    toEnum 1 = WordsOnlyOutput
    toEnum 2 = MDFOutputWithEtymons
    toEnum 3 = WordsWithProtoOutput
    toEnum _ = undefined

-- | Output of a single application of rules to a wordlist: either a
-- list of possibly highlighted words, an applied rules table, or a
-- parse error.
data ApplicationOutput a r
    = HighlightedWords [Component (a, Bool)]
    | AppliedRulesTable [PWordLog r]
    | ParseError (ParseErrorBundle String Void)
    deriving (Show, Generic, NFData)

-- | Kind of input: either a raw wordlist, or an MDF file.
data InputLexiconFormat = Raw | MDF
    deriving (Show, Eq)
instance Enum InputLexiconFormat where
    -- used for conversion to and from C, so want control over values
    fromEnum Raw = 0
    fromEnum MDF = 1

    toEnum 0 = Raw
    toEnum 1 = MDF
    toEnum _ = undefined

data ParseOutput a = ParsedRaw [Component a] | ParsedMDF SFM
    deriving (Show, Functor, Foldable, Traversable)

componentise :: OutputMode -> [a] -> [Component a] -> [Component a]
componentise WordsWithProtoOutput ws cs = intersperseWords ws cs
componentise _                    _  cs = cs

intersperseWords :: [a] -> [Component a] -> [Component a]
intersperseWords (w:ws) (Word c:cs) =
    Word w : Separator " → " : Word c : Separator "\n" : intersperseWords ws cs
intersperseWords ws (_:cs) = intersperseWords ws cs
intersperseWords [] cs = cs
intersperseWords _ [] = []

tokeniseAccordingToInputFormat
    :: InputLexiconFormat
    -> OutputMode
    -> SoundChanges Expanded [Grapheme]
    -> String
    -> Either (ParseErrorBundle String Void) [Component PWord]
tokeniseAccordingToInputFormat Raw _ cs =
    withFirstCategoriesDecl tokeniseWords cs
tokeniseAccordingToInputFormat MDF MDFOutputWithEtymons cs =
    withFirstCategoriesDecl tokeniseMDF cs <=<
    -- TODO don't hard-code hierarchy and filename
    fmap (fromTree . duplicateEtymologies ('*':) . toTree mdfHierarchy)
    . parseSFM ""
tokeniseAccordingToInputFormat MDF o cs = \input -> do
    sfm <- parseSFM "" input
    ws <- withFirstCategoriesDecl tokeniseMDF cs sfm
    pure $ case o of
        MDFOutput -> ws
        _ ->  -- need to extract words for other output modes
            Word <$> getWords ws

-- | Top-level dispatcher for an interactive frontend: given a textual
-- wordlist and a list of sound changes, returns the result of running
-- the changes in the specified mode.
parseTokeniseAndApplyRules
    :: (forall a b. (a -> b) -> [Component a] -> [Component b])  -- ^ mapping function to use (for parallelism)
    -> SoundChanges Expanded [Grapheme] -- ^ changes
    -> String       -- ^ words
    -> InputLexiconFormat
    -> ApplicationMode
    -> Maybe [Component PWord]  -- ^ previous results
    -> ApplicationOutput PWord (Statement Expanded [Grapheme])
parseTokeniseAndApplyRules parFmap statements ws intype mode prev =
    case tokeniseAccordingToInputFormat intype (getOutputMode mode) statements ws of
        Left e -> ParseError e
        Right toks
          | ws' <- getWords toks
          -> case mode of
            ReportRulesApplied ->
                AppliedRulesTable $ mapMaybe toPWordLog $ concat $
                    getWords $ componentise WordsOnlyOutput [] $
                        parFmap (applyChangesWithLog statements) toks
            ApplyRules DifferentToLastRun mdfout sep ->
                let result = concatMap (splitMultipleResults sep) $
                      componentise mdfout (fmap pure ws') $
                          parFmap (applyChanges statements) toks
                in HighlightedWords $
                    zipWithComponents result (fromMaybe [] prev) [] $ \thisWord prevWord ->
                        (thisWord, thisWord /= prevWord)
            ApplyRules DifferentToInput mdfout sep ->
                HighlightedWords $ concatMap (splitMultipleResults sep) $
                    (fmap.fmap) (mapMaybe extractMaybe) $
                        componentise mdfout (fmap (pure . first Just . (,False)) ws') $
                            parFmap (applyChangesWithChanges statements) toks
            ApplyRules NoHighlight mdfout sep ->
                HighlightedWords $ (fmap.fmap) (,False) $ concatMap (splitMultipleResults sep) $
                    componentise mdfout (fmap pure ws') $
                        parFmap (applyChanges statements) toks
  where
    -- Zips two tokenised input strings. Compared to normal 'zipWith'
    -- this has two special properties:
    --
    --   * It only zips v'Word's. Any non-v'Word's in the first argument
    --     will be passed unaltered to the output; any in the second
    --     argument will be ignored.
    --
    --   * The returned list will have the same number of elements as does
    --     the first argument. If a v'Word' in the first argument has no
    --     corresponding v'Word' in the second, the zipping function is
    --     called using the default @b@ value given as the third argument.
    --     Such a v'Word' in the second argument will simply be ignored.
    --
    -- Note the persistent assymetry in the definition: each 'Component'
    -- in the first argument will be reflected in the output, but each in
    -- the second argument may be ignored.
    zipWithComponents :: [Component a] -> [Component b] -> b -> (a -> b -> c) -> [Component c]
    zipWithComponents []             _            _  _ = []
    zipWithComponents as            []            bd f = (fmap.fmap) (`f` bd) as
    zipWithComponents (Word a:as)   (Word b:bs)   bd f = Word (f a b) : zipWithComponents as bs bd f
    zipWithComponents as@(Word _:_) (_:bs)        bd f = zipWithComponents as bs bd f
    zipWithComponents (a:as)        bs@(Word _:_) bd f = unsafeCastComponent a : zipWithComponents as bs bd f
    zipWithComponents (a:as)        (_:bs)        bd f = unsafeCastComponent a : zipWithComponents as bs bd f

    unsafeCastComponent :: Component a -> Component b
    unsafeCastComponent (Word _) = error "unsafeCastComponent: attempted to cast a word!"
    unsafeCastComponent (Separator s) = Separator s
    unsafeCastComponent (Gloss s) = Gloss s

    extractMaybe (Just a, b) = Just (a, b)
    extractMaybe (Nothing, _) = Nothing
