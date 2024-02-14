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

import Data.Bifunctor (second)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Void (Void)
import GHC.Generics (Generic)

import Control.DeepSeq (NFData)
import Text.Megaparsec (ParseErrorBundle)

import Brassica.MDF (MDF, parseMDFWithTokenisation, componentiseMDF, componentiseMDFWordsOnly, duplicateEtymologies)
import Brassica.SoundChange.Apply
import Brassica.SoundChange.Apply.Internal (applyChangesWithLog, toPWordLog)
import Brassica.SoundChange.Category
import Brassica.SoundChange.Tokenise
import Brassica.SoundChange.Types

-- | Rule application mode of the SCA.
data ApplicationMode
    = ApplyRules HighlightMode OutputMode String
    | ReportRulesApplied
    deriving (Show, Eq)

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

data TokenisationMode = Normal | AddEtymons
    deriving (Show, Eq)
instance Enum TokenisationMode where
    -- used for conversion to and from C, so want control over values
    fromEnum Normal = 0
    fromEnum AddEtymons = 1

    toEnum 0 = Normal
    toEnum 1 = AddEtymons
    toEnum _ = undefined

tokenisationModeFor :: ApplicationMode -> TokenisationMode
tokenisationModeFor (ApplyRules _ MDFOutputWithEtymons _) = AddEtymons
tokenisationModeFor _ = Normal

-- | Output of a single application of rules to a wordlist: either a
-- list of possibly highlighted words, an applied rules table, or a
-- parse error.
data ApplicationOutput a r
    = HighlightedWords [Component (a, Bool)]
    | AppliedRulesTable [PWordLog r]
    | ParseError (ParseErrorBundle String Void)
    | ExpandError ExpandError
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

data ParseOutput a = ParsedRaw [Component a] | ParsedMDF (MDF [Component a])
    deriving (Show, Functor, Foldable, Traversable)

componentise :: OutputMode -> [a] -> ParseOutput a -> [Component a]
componentise WordsWithProtoOutput ws (ParsedRaw cs) = intersperseWords ws cs
componentise _                    _ (ParsedRaw cs) = cs
componentise MDFOutput            _ (ParsedMDF mdf) = componentiseMDF mdf
componentise MDFOutputWithEtymons _ (ParsedMDF mdf) = componentiseMDF mdf
componentise WordsOnlyOutput      _ (ParsedMDF mdf) = componentiseMDFWordsOnly mdf
componentise WordsWithProtoOutput ws (ParsedMDF mdf) = intersperseWords ws $ componentiseMDFWordsOnly mdf

intersperseWords :: [a] -> [Component a] -> [Component a]
intersperseWords (w:ws) (Word c:cs) =
    Word w : Separator " → " : Word c : Separator "\n" : intersperseWords ws cs
intersperseWords ws (_:cs) = intersperseWords ws cs
intersperseWords [] cs = cs
intersperseWords _ [] = []

tokeniseAccordingToInputFormat
    :: InputLexiconFormat
    -> TokenisationMode
    -> SoundChanges Expanded [Grapheme]
    -> String
    -> Either (ParseErrorBundle String Void) (ParseOutput PWord)
tokeniseAccordingToInputFormat Raw _ cs =
    fmap ParsedRaw . withFirstCategoriesDecl tokeniseWords cs
tokeniseAccordingToInputFormat MDF Normal cs =
    fmap ParsedMDF . withFirstCategoriesDecl parseMDFWithTokenisation cs
tokeniseAccordingToInputFormat MDF AddEtymons cs =
    fmap ParsedMDF
    . second (duplicateEtymologies $ ('*':) . detokeniseWords)
    . withFirstCategoriesDecl parseMDFWithTokenisation cs

getParsedWords :: ParseOutput a -> [a]
getParsedWords (ParsedRaw cs) = getWords cs
getParsedWords (ParsedMDF mdf) = getWords $ componentiseMDF mdf

-- | Top-level dispatcher for an interactive frontend: given a textual
-- wordlist and a list of sound changes, returns the result of running
-- the changes in the specified mode.
parseTokeniseAndApplyRules
    :: (forall a b. (a -> b) -> ParseOutput a -> ParseOutput b)  -- ^ mapping function to use (for parallelism)
    -> SoundChanges CategorySpec Directive -- ^ changes
    -> String       -- ^ words
    -> InputLexiconFormat
    -> ApplicationMode
    -> Maybe [Component PWord]  -- ^ previous results
    -> ApplicationOutput PWord (Statement Expanded [Grapheme])
parseTokeniseAndApplyRules parFmap statements ws intype mode prev =
    case expandSoundChanges statements of
        Left e -> ExpandError e
        Right statements' ->
            let tmode = tokenisationModeFor mode in
            case tokeniseAccordingToInputFormat intype tmode statements' ws of
                Left e -> ParseError e
                Right toks
                  | ws' <- getParsedWords toks
                  -> case mode of
                    ReportRulesApplied ->
                        AppliedRulesTable $ mapMaybe toPWordLog $ concat $
                            getWords $ componentise WordsOnlyOutput [] $
                                parFmap (applyChangesWithLog statements') toks
                    ApplyRules DifferentToLastRun mdfout sep ->
                        let result = concatMap (splitMultipleResults sep) $
                              componentise mdfout (fmap pure ws') $
                                  parFmap (applyChanges statements') toks
                        in HighlightedWords $
                            zipWithComponents result (fromMaybe [] prev) [] $ \thisWord prevWord ->
                                (thisWord, thisWord /= prevWord)
                    ApplyRules DifferentToInput mdfout sep ->
                        HighlightedWords $ concatMap (splitMultipleResults sep) $
                            componentise mdfout (fmap (pure . (,False)) ws') $
                                parFmap (applyChangesWithChanges statements') toks
                    ApplyRules NoHighlight mdfout sep ->
                        HighlightedWords $ (fmap.fmap) (,False) $ concatMap (splitMultipleResults sep) $
                            componentise mdfout (fmap pure ws') $
                                parFmap (applyChanges statements') toks
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
