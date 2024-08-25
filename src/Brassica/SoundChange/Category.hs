{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}

module Brassica.SoundChange.Category
       ( Categories
       , AutosegmentDef(..)
       , Brassica.SoundChange.Category.lookup
       , values
       , ExpandError(..)
       , expand
       , expandRule
       , extendCategories
       , expandSoundChanges
       ) where

import Prelude hiding (lookup)
import Control.DeepSeq (NFData)
import Control.Monad (foldM, unless, zipWithM)
import Control.Monad.State.Strict (StateT, evalStateT, lift, get, put, gets)
import Data.Bifunctor (first, second)
import Data.Containers.ListUtils (nubOrd)
import Data.List (intersect, transpose, foldl')
import Data.Maybe (mapMaybe, catMaybes)
import Data.Traversable (for)
import GHC.Generics (Generic)

import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as M

import Brassica.SoundChange.Types

-- | Expanding an autosegment from a grapheme requires knowing its
-- feature name, and a set of graphemes cross-cutting that feature.
data AutosegmentDef = AutosegmentDef
    { autoFeature :: String
    , autoGraphemes :: [String]
    }
    deriving (Eq, Show)

-- | A map from names to the (expanded) categories or autosegments
-- they reference. Used to resolve cross-references between
-- categories.
type Categories = M.Map String (Either (Expanded 'AnyPart) AutosegmentDef)

-- | Lookup a category name in 'Categories'.
lookup :: String -> Categories -> Maybe (Either (Expanded a) AutosegmentDef)
lookup = (fmap (first generaliseExpanded) .) . M.lookup

-- | Returns a list of every value mentioned in a set of
-- 'Categories'
values :: Categories -> [Either Grapheme [Lexeme Expanded 'AnyPart]]
values = nubOrd . concatMap (either elements autoElements) . M.elems
  where
    autoElements = fmap (Left . GMulti) . autoGraphemes

-- Errors which can be emitted while inlining or expanding category
-- definitions.
data ExpandError
    = NotFound String
      -- ^ A category with that name was not found
    | InvalidBaseValue
      -- ^ A 'Lexeme' was used as a base value in a feature
    | InvalidDerivedValue
      -- ^ A 'Lexeme' was used as a derived value in an autosegment
    | InvalidAuto String
      -- ^ A bad category was used in an autosegment declaration
    | MismatchedLengths
      -- ^ A 'FeatureSpec' contained a mismatched number of values
    deriving (Show, Generic, NFData)

-- | Given a category, return the list of values which it
-- matches.
expand :: Categories -> CategorySpec a -> Either ExpandError (Expanded a)
expand cs (MustInline g) = case lookup g cs of
    Just (Left expanded) -> Right expanded
    _ -> Left $ NotFound g
expand cs (CategorySpec spec) = FromElements <$> foldM go [] spec
  where
    go es (modifier, e) = do
        (new, modifier') <- case e of
            Left (GMulti g)
                | Just (g', '~') <- unsnoc g
                    -> pure ([Left (GMulti g')], modifier)
                | modifier == Intersect
                , Just (Left (FromElements c)) <- lookup ('+':g) cs
                    -> pure (c, Intersect)
                | modifier == Subtract
                , Just (Left (FromElements c)) <- lookup ('-':g) cs
                    -> pure (c, Intersect)  -- do intersection with negative instead!
                | Just (Left (FromElements c)) <- lookup g cs
                    -> pure (c, modifier)
                | Just (Right _) <- lookup g cs
                    -- re-expand to produce appropriate 'Auto'
                    -> (,modifier) . pure . Right . pure <$> expandLexeme cs (Grapheme (GMulti g))

                    -- Note: there are other options for design here
                    -- see https://verduria.org/viewtopic.php?p=85766#p85766
                    -- | Just (Right (AutosegmentDef _ gs)) <- lookup g cs
                    -- 1. -> pure ([Left (GMulti g)], modifier)
                    -- 2. -> pure (Left . GMulti <$> g:gs, modifier)
                | otherwise -> pure ([Left (GMulti g)], modifier)
            Left GBoundary -> pure ([Left GBoundary], modifier)
            Right ls -> (,modifier) . pure . Right <$> traverse (expandLexeme cs) ls
        pure $ case modifier' of
            Union -> es ++ new
            -- important: intersection preserves order of the /last/ category mentioned!
            Intersect -> new `intersect` es
            Subtract -> es `subtractAll` new

    -- NB. normal (\\) only removes the first matching element
    subtractAll xs ys = filter (`notElem` ys) xs

expandLexeme :: Categories -> Lexeme CategorySpec a -> Either ExpandError (Lexeme Expanded a)
expandLexeme cs (Grapheme (GMulti g))
    | Just (g', '~') <- unsnoc g
        = Right $ Grapheme $ GMulti g'
    | otherwise =
        case lookup g cs of
            Just (Left c) -> Right $ Category c
            Just (Right a) -> do
                kvs <- expandFeature cs (autoFeature a)
                pure $ Autosegment (autoFeature a) kvs (g : autoGraphemes a)
            Nothing -> Right $ Grapheme (GMulti g)
expandLexeme _  (Grapheme GBoundary) = Right $ Grapheme GBoundary
expandLexeme cs (Category c) = Category <$> expand cs c
expandLexeme cs (Optional ls) = Optional <$> traverse (expandLexeme cs) ls
expandLexeme _  Metathesis = Right Metathesis
expandLexeme _  Geminate = Right Geminate
expandLexeme cs (Wildcard l) = Wildcard <$> expandLexeme cs l
expandLexeme cs (Kleene l) = Kleene <$> expandLexeme cs l
expandLexeme _  Discard = Right Discard
expandLexeme cs (Backreference i c) = Backreference i <$> expand cs c
expandLexeme cs (Multiple c) = Multiple <$> expand cs c
expandLexeme cs (Feature n i [] l) = do
    kvs <- expandFeature cs n
    l' <- expandLexeme cs l
    pure $ Feature n i kvs l'
expandLexeme cs (Feature n i kvs l) = Feature n i kvs <$> expandLexeme cs l
expandLexeme _  (Autosegment n kvs gs) =
    -- in reality this case should never occur from parsed sound changes
    pure $ Autosegment n kvs gs

expandFeature
    :: Categories
    -> String
    -> Either ExpandError [(String, String)]
expandFeature cs n =
    case M.lookup ('+':n) cs of
        Just (Left (FromElements positive)) ->
            case M.lookup ('-':n) cs of
                Just (Left (FromElements negative))
                    | length positive /= length negative -> Left MismatchedLengths
                    | Just positive' <- traverse getBaseValue positive
                    , Just negative' <- traverse getBaseValue negative
                    -> Right $ zip negative' positive'
                    | otherwise -> Left InvalidBaseValue
                _ -> Left $ NotFound ('-':n)
        _ -> Left $ NotFound ('+':n)

getBaseValue :: Either Grapheme [Lexeme Expanded 'AnyPart] -> Maybe String
getBaseValue (Left (GMulti g)) = Just g
getBaseValue _ = Nothing

-- taken from base-4.19
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
{-# INLINABLE unsnoc #-}

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
               (\base ds -> (base, FromElements $ Left (GMulti base) : ds))
               baseValues'
               (transpose derivedValues)
            newCats = fmap (second Left) $
                maybe [] (pure . (,baseValues)) (featureBaseName spec)
                ++ derivedCats
                ++ features
        Right $ foldl' (flip $ uncurry M.insert) cs newCats
    go cs (DefineAuto name) = do
        (stated, other, feature) <- case name of
            '+':rest | Just (Left (FromElements p)) <- M.lookup name cs
                -> case M.lookup ('-':rest) cs of
                       Just (Left (FromElements n)) -> Right (p, n, rest)
                       _ -> Left $ NotFound ('-':rest)
            '-':rest | Just (Left (FromElements n)) <- M.lookup name cs
                -> case M.lookup ('+':rest) cs of
                       Just (Left (FromElements p)) -> Right (n, p, rest)
                       _ -> Left $ NotFound ('+':rest)
            _ -> Left $ InvalidAuto name
        autoCs <- M.fromList <$> zipWithM (mkAuto feature) stated other
        pure $ M.merge
            M.preserveMissing M.preserveMissing
            (M.zipWithMatched $ \_ c _ -> c)  -- prefer categories to autosegments
            cs autoCs

    mkAuto
        :: String
        -> Either Grapheme [Lexeme Expanded 'AnyPart]
        -> Either Grapheme [Lexeme Expanded 'AnyPart]
        -> Either ExpandError (String, Either (Expanded 'AnyPart) AutosegmentDef)
    mkAuto f (Left (GMulti g)) e
        | Left (GMulti g') <- e = Right (g, Right $ AutosegmentDef f [g'])
        | otherwise = Left InvalidDerivedValue
    mkAuto _ _ _ = Left InvalidBaseValue

expandSoundChanges
    :: SoundChanges CategorySpec Directive
    -> Either ExpandError (SoundChanges Expanded [Grapheme])
expandSoundChanges = fmap catMaybes . flip evalStateT (M.empty, []) . traverse go
  where
    go  :: Statement CategorySpec Directive
        -> StateT
            (Categories, [String])
            (Either ExpandError)
            (Maybe (Statement Expanded [Grapheme]))
    go (RuleS r) = do
        cs <- gets fst
        lift $ Just . RuleS <$> expandRule cs r
    go (FilterS f) = do
        cs <- gets fst
        lift $ Just . FilterS <$> expandFilter cs f
    go ReportS = pure (Just ReportS)
    go (DirectiveS (ExtraGraphemes extra)) = do
        (cs, _) <- get
        put (cs, extra)
        pure Nothing
    go (DirectiveS (Categories overwrite noreplace defs)) = do
        (cs, extra) <- get
        cs' <- lift $ extendCategories cs (overwrite, defs)
        put (cs', extra)
        pure $ if noreplace
            then Nothing
            else Just $ DirectiveS $ fmap GMulti extra ++ mapMaybe left (values cs')

    left (Left l) = Just l
    left (Right _) = Nothing
