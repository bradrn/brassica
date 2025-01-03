{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Brassica.SoundChange.Expand
-- Copyright   : See LICENSE file
-- License     : BSD3
-- Maintainer  : Brad Neimann
--
-- This module implements the process of /expansion/, from Brassica’s
-- surface syntax to a simpler representation in which all categories,
-- features and autosegments have been inlined. For further
--
-- In the surface syntax, each category is represented as a
-- 'CategorySpec', a description in terms of predefined categories
-- combined with category operations. Expansion converts each one to
-- an 'Expanded' list of graphemes.
--
-- Similarly, category definitions are parsed as 'Directive's. Once
-- inlined, these can be replaced with simple 'GraphemeList's to be
-- used for filtering graphemes.
module Brassica.SoundChange.Expand
       (
       -- * Main function
         expandSoundChanges
       , ExpandError(..)
       -- * Expanding individual elements
       , expand
       , expandRule
       , extendCategories
       -- * Categories
       , Categories
       , AutosegmentDef(..)
       , Brassica.SoundChange.Expand.lookup
       , values
       ) where

import Prelude hiding (lookup)
import Control.DeepSeq (NFData)
import Control.Monad (foldM, unless)
import Control.Monad.State.Strict (StateT, evalStateT, lift, get, put, gets)
import Data.Bifunctor (first, second)
import Data.Containers.ListUtils (nubOrd)
import Data.List (transpose, foldl', stripPrefix)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Traversable (for)
import GHC.Generics (Generic)

import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as M

import Brassica.SoundChange.Types

-- | Expanding an autosegment from a grapheme requires knowing its
-- feature name, and a set of graphemes cross-cutting that feature.
-- (Note that 'autoGraphemes' includes the originally-written
-- grapheme.)
data AutosegmentDef = AutosegmentDef
    { autoFeature :: String
    , autoGraphemes :: [String]
    }
    deriving (Eq, Show)

-- | A map from names to the (expanded) categories or autosegments
-- they reference. Used to resolve cross-references between
-- categories.
type Categories = M.Map String (Either (Expanded 'AnyPart) AutosegmentDef)

-- | Lookup a category name in t'Categories'.
lookup :: String -> Categories -> Maybe (Either (Expanded a) AutosegmentDef)
lookup = (fmap (first generaliseExpanded) .) . M.lookup

-- | Returns a list of every value mentioned in a set of
-- t'Categories'
values :: Categories -> [[Lexeme Expanded 'AnyPart]]
values = nubOrd . concatMap (either elements autoElements) . M.elems
  where
    autoElements = fmap (pure . Grapheme) . autoGraphemes

-- | Errors which can be emitted while inlining or expanding category
-- definitions.
data ExpandError
    = NotFound String
      -- ^ A category with the given name was not found
    | InvalidBaseValue
      -- ^ A 'Lexeme' was used as a base value in a feature
    | InvalidDerivedValue
      -- ^ A 'Lexeme' was used as a derived value in an autosegment
    | MismatchedLengths
      -- ^ A phonetic feature or 'FeatureSpec' contained a mismatched number of values
    deriving (Show, Generic, NFData)

-- | Given an unexpanded category, return the list of values which it
-- matches.
expand :: Categories -> CategorySpec a -> Either ExpandError (Expanded a)
expand cs (MustInline g) = case lookup g cs of
    Just (Left expanded) -> Right expanded
    _ -> Left $ NotFound g
expand cs (CategorySpec spec) = FromElements <$> foldM go [] spec
  where
    go :: [CategoryElement Expanded a]
                  -> (CategoryModification, CategoryElement CategorySpec a)
                  -> Either ExpandError [CategoryElement Expanded a]
    go es (modifier, e) = do
        (new, modifier') <- case e of
            [Grapheme g]
                | Just (g', '~') <- unsnoc g
                    -> pure ([[Grapheme g']], modifier)
                | modifier == Intersect
                , Just (Left (FromElements c)) <- lookup ('+':g) cs
                    -> pure (c, Intersect)
                | modifier == Subtract
                , Just (Left (FromElements c)) <- lookup ('-':g) cs
                    -> pure (c, Intersect)  -- do intersection with negative instead!
                | '&':g' <- g
                , Just (Left (FromElements p)) <- lookup ('+':g') cs
                , Just (Left (FromElements n)) <- lookup ('-':g') cs
                    -> pure (n++p, modifier)
                | Just (Left (FromElements c)) <- lookup g cs
                    -> pure (c, modifier)
                | Just (Right _) <- lookup g cs
                    -- re-expand to produce appropriate 'Auto'
                    -> (,modifier) . pure . pure <$> expandLexeme cs (Grapheme g)

                    -- Note: there are other options for design here
                    -- see https://verduria.org/viewtopic.php?p=85766#p85766
                    -- | Just (Right (AutosegmentDef _ gs)) <- lookup g cs
                    -- 1. -> pure ([Left (GMulti g)], modifier)
                    -- 2. -> pure (Left . GMulti <$> g:gs, modifier)
                | otherwise -> pure ([[Grapheme g]], modifier)
            ls -> (,modifier) . pure <$> traverse (expandLexeme cs) ls
        pure $ case modifier' of
            Union -> es ++ new
            -- important: intersection preserves order of the /last/ category mentioned!
            Intersect -> es `intersectC` new
            Subtract -> es `subtractC` new

    -- Set operations, also looking into 'Autosegment's
    subtractC, intersectC
        :: [[Lexeme Expanded a]]
        -> [[Lexeme Expanded a]]
        -> [[Lexeme Expanded a]]

    subtractC es new = mapMaybe go' es
      where
        go' g | g `elemAuto` new = Nothing
        go' [Autosegment n kvs gs] =
            Just [Autosegment n
                  (filterkvs (`notElem` new) kvs)
                  (filter ((`notElem` new) . pure . Grapheme) gs)]
        go' g = Just g

    intersectC es new = mapMaybe go' new
      where
        go' g | g `elemAuto` es = Just g
        go' [Autosegment n kvs gs] =
            Just [Autosegment n
                  (filterkvs (`elem` es) kvs)
                  (filter ((`elem` es) . pure . Grapheme) gs)]
        go' _ = Nothing

    elemAuto :: [Lexeme Expanded a] -> [[Lexeme Expanded a]] -> Bool
    elemAuto _ [] = False
    elemAuto g'@[Grapheme gm] ([Autosegment _ _ gs]:ls) = (gm `elem` gs) || elemAuto g' ls
    elemAuto g' (g:ls) = (g' == g) || elemAuto g' ls

    filterkvs :: ([Lexeme Expanded a] -> Bool) -> [[(Grapheme, Bool)]] -> [[(Grapheme, Bool)]]
    filterkvs p = fmap . fmap $ \(g, b) -> (g, b && p [Grapheme g])

expandLexeme :: Categories -> Lexeme CategorySpec a -> Either ExpandError (Lexeme Expanded a)
expandLexeme cs (Grapheme g)
    | Just (g', '~') <- unsnoc g
        = Right $ Grapheme g'
    | otherwise =
        case lookup g cs of
            Just (Left c) -> Right $ Category c
            Just (Right a) -> do
                kvs <- expandFeature cs (autoFeature a)
                pure $ Autosegment (autoFeature a) ((fmap.fmap) (,True) kvs) (autoGraphemes a)
            Nothing -> Right $ Grapheme g
expandLexeme cs (Category c) = Category <$> expand cs c
expandLexeme cs (GreedyCategory c) = GreedyCategory <$> expand cs c
expandLexeme cs (Optional ls) = Optional <$> traverse (expandLexeme cs) ls
expandLexeme cs (GreedyOptional ls) = GreedyOptional <$> traverse (expandLexeme cs) ls
expandLexeme _  Metathesis = Right Metathesis
expandLexeme _  Geminate = Right Geminate
expandLexeme cs (Wildcard l) = Wildcard <$> expandLexeme cs l
expandLexeme cs (Kleene l) = Kleene <$> expandLexeme cs l
expandLexeme _  Discard = Right Discard
expandLexeme cs (Backreference i c) = Backreference i <$> expand cs c
expandLexeme cs (Multiple c) = Multiple <$> expand cs c
expandLexeme cs (Feature r n i [] l) = do
    kvs <- expandFeature cs n
    l' <- expandLexeme cs l
    pure $ Feature r n i kvs l'
expandLexeme cs (Feature r n i kvs l) = Feature r n i kvs <$> expandLexeme cs l
expandLexeme _  (Autosegment n kvs gs) =
    -- in reality this case should never occur from parsed sound changes
    pure $ Autosegment n kvs gs

expandFeature :: Categories -> String -> Either ExpandError [[String]]
expandFeature cs n = transpose . fmap snd <$> lookupFeature cs n

lookupFeature
    :: Categories
    -> String  -- ^ Feature name (no +/- prefix or +value suffix)
    -> Either ExpandError [(String, [String])]
lookupFeature cs n =
    let pluss :: M.Map String (String, Either (Expanded 'AnyPart) AutosegmentDef)
        pluss = M.mapMaybeWithKey plusPrefix cs
    -- NB. consistency is guaranteed as 'elems' always returns items in ascending order
    in case M.elems pluss of
        [] -> Left $ NotFound ('+':n)
        [("", Left (FromElements positive))] ->
            case M.lookup ('-':n) cs of
                Just (Left (FromElements negative))
                    | length positive /= length negative -> Left MismatchedLengths
                    | Just positive' <- traverse getBaseValue positive
                    , Just negative' <- traverse getBaseValue negative
                    -> Right [("-", negative'), ("+", positive')]
                    | otherwise -> Left InvalidBaseValue
                _ -> Left $ NotFound ('-':n)
        kvs -> case traverse getCategory kvs of
            Just vs@(v:vs')
                | any ((length v /=) . length) vs' -> Left MismatchedLengths
                | Just vs'' <- traverse (traverse getBaseValue) vs
                -> Right $ zip (fst <$> kvs) vs''
            _ -> Left InvalidBaseValue
  where
    plusPrefix ('+':k) v = case stripPrefix n k of
        Just ('+':k') -> Just (k', v)
        Just "" -> Just ("", v)
        _ -> Nothing
    plusPrefix _ _ = Nothing

    getCategory (_, Left (FromElements c)) = Just c
    getCategory _ = Nothing

getBaseValue :: CategoryElement Expanded 'AnyPart -> Maybe String
getBaseValue [Grapheme g] = Just g
getBaseValue _ = Nothing

-- taken from base-4.19
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
{-# INLINABLE unsnoc #-}

-- | Expand all categories in a given sound change 'Rule'.
expandRule :: Categories -> Rule CategorySpec -> Either ExpandError (Rule Expanded)
expandRule cs r = Rule
    <$> traverse (expandLexeme cs) (target r)
    <*> traverse (expandLexeme cs) (replacement r)
    <*> traverse expandEnvironment (environment r)
    <*> traverse expandEnvironment (exception r)
    <*> pure (flags r)
    <*> pure (plaintext r)
  where
    expandEnvironment (e1, e2) = (,)
        <$> traverse (expandLexeme cs) e1
        <*> traverse (expandLexeme cs) e2

expandFilter :: Categories -> Filter CategorySpec -> Either ExpandError (Filter Expanded)
expandFilter cs (Filter p f) = Filter p <$> traverse (expandLexeme cs) f

-- | Extend a set of previously defined t'Categories' to give the
-- resulting state after a v'Categories' directive.
extendCategories
    :: Categories
    -> (Bool, [CategoryDefinition])  -- ^ The fields of a v'Categories' directive
    -> Either ExpandError Categories
extendCategories cs' (overwrite, defs) =
    foldM go (if overwrite then M.empty else cs') defs
  where
    go :: Categories -> CategoryDefinition -> Either ExpandError Categories
    go cs (DefineCategory name val) = flip (M.insert name) cs . Left <$> expand cs val
    go cs (DefineFeature spec) = do
        baseValues <- expand cs $ featureBaseValues spec
        derivedCats <- traverse (traverse $ expand cs) $ featureDerived spec

        baseValues' <- for (elements baseValues) $
            maybe (Left InvalidBaseValue) Right . getBaseValue
        let baseLen = length baseValues'
            derivedValues = elements . snd <$> derivedCats
        unless (all ((==baseLen) . length) derivedValues) $
            Left MismatchedLengths

        let features = zipWith
               (\base ds -> (base, FromElements $ [Grapheme base] : ds))
               baseValues'
               (transpose derivedValues)
            newCats = fmap (second Left) $
                maybe [] (pure . (,baseValues)) (featureBaseName spec)
                ++ derivedCats
                ++ features
        Right $ foldl' (flip $ uncurry M.insert) cs newCats
    go cs (DefineAuto catName) = do
        let (featureName, featureValue) = case catName of
                '-':n -> (n, "-")
                '+':n -> case break (=='+') n of
                    (prefix, '+':suffix) -> (prefix, suffix)
                    (_, []) -> (n, "+")
                    _ -> error "extendCategories: unexpected output from 'break'"
                n -> (n, "")  -- let it error out below
        features <- M.fromList <$> lookupFeature cs featureName
        case M.lookup featureValue features of
            Nothing -> Left $ NotFound catName
            Just gs ->
                -- NB. consistency is guaranteed as 'elems' always returns items in ascending order
                let autoCs = M.fromList $
                        zipWith (mkAuto featureName) gs (transpose $ M.elems features)
                in pure $ M.merge
                    M.preserveMissing M.preserveMissing
                    (M.zipWithMatched $ \_ _ c -> c)
                    cs autoCs

    mkAuto :: String -> String -> [String] -> (String, Either (Expanded 'AnyPart) AutosegmentDef)
    mkAuto f g gs = (g, Right $ AutosegmentDef f gs)

-- | Expand a set of 'SoundChanges'. Expansion proceeds from beginning
-- to end as follows:
--
--     * Rules and filters are expanded by expanding all categories
--       within them (with 'expand'). Graphemes are replaced with
--       categories or autosegments if previously defined as such.
--
--     * If a v'Categories' definition block is found, the categories
--       defined within it are expanded and added to (or replace) the
--       list of current categories. The block is replaced with a list
--       of currently defined graphemes.
--
--     * If 'ExtraGraphemes' are found, they are added to a list of
--       currently defined graphemes. They are replaced with a
--       'GraphemeList' only if no categories are defined in the
--       'SoundChanges'.
expandSoundChanges
    :: SoundChanges CategorySpec Directive
    -> Either ExpandError (SoundChanges Expanded GraphemeList)
expandSoundChanges scs = fmap catMaybes $ flip evalStateT (M.empty, []) $ traverse go scs
  where
    noCategories = any (\case DeclS (Categories {}) -> True; _ -> False) scs

    go  :: Statement CategorySpec Directive
        -> StateT
            (Categories, [String])
            (Either ExpandError)
            (Maybe (Statement Expanded GraphemeList))
    go (RuleS r) = do
        cs <- gets fst
        lift $ Just . RuleS <$> expandRule cs r
    go (FilterS f) = do
        cs <- gets fst
        lift $ Just . FilterS <$> expandFilter cs f
    go ReportS = pure (Just ReportS)
    go (DeclS (ExtraGraphemes extra)) = do
        (cs, _) <- get
        put (cs, extra)
        pure $
            if noCategories
            then Just $ DeclS $ GraphemeList True extra
            else Nothing
    go (DeclS (Categories overwrite noreplace defs)) = do
        (cs, extra) <- get
        cs' <- lift $ extendCategories cs (overwrite, defs)
        put (cs', extra)
        pure $ Just $ DeclS $ GraphemeList noreplace $ extra ++ mapMaybe grapheme (values cs')

    grapheme [Grapheme g] = Just g
    grapheme _ = Nothing
