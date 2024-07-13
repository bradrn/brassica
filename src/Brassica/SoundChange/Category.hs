{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Brassica.SoundChange.Category
       ( Categories
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
import Control.Monad (foldM, unless)
import Control.Monad.State.Strict (StateT, evalStateT, lift, get, put, gets)
import Data.Containers.ListUtils (nubOrd)
import Data.List (intersect, transpose, foldl')
import Data.Maybe (mapMaybe, catMaybes)
import GHC.Generics (Generic)

import qualified Data.Map.Strict as M

import Brassica.SoundChange.Types
import Data.Traversable (for)

-- | A map from names to the (expanded) categories they
-- reference. Used to resolve cross-references between categories.
type Categories = M.Map String (Expanded 'AnyPart)

-- | Lookup a category name in 'Categories'.
lookup :: String -> Categories -> Maybe (Expanded a)
lookup = (fmap generaliseExpanded .) . M.lookup

-- | Returns a list of every value mentioned in a set of
-- 'Categories'
values :: Categories -> [Either Grapheme [Lexeme Expanded 'AnyPart]]
values = nubOrd . concatMap elements . M.elems

-- Errors which can be emitted while inlining or expanding category
-- definitions.
data ExpandError
    = NotFound String
      -- ^ A category with that name was not found
    | InvalidBaseValue
      -- ^ A 'Lexeme' was used as a base value in a feature
    | MismatchedLengths
      -- ^ A 'FeatureSpec' contained a mismatched number of values
    deriving (Show, Generic, NFData)

-- | Given a category, return the list of values which it
-- matches.
expand :: Categories -> CategorySpec a -> Either ExpandError (Expanded a)
expand cs (MustInline g) = maybe (Left $ NotFound g) Right $ lookup g cs
expand cs (CategorySpec spec) = FromElements <$> foldM go [] spec
  where
    go es (modifier, e) = do
        new <- case e of
            Left (GMulti g)
                | Just (FromElements c) <- lookup g cs
                -> pure c
                | otherwise -> pure [Left (GMulti g)]
            Left GBoundary -> pure [Left GBoundary]
            Right ls -> pure . Right <$> traverse (expandLexeme cs) ls
        pure $ case modifier of
            Union -> es ++ new
            Intersect -> es `intersect` new
            Subtract -> es `subtractAll` new

    -- NB. normal (\\) only removes the first matching element
    subtractAll xs ys = filter (`notElem` ys) xs

expandLexeme :: Categories -> Lexeme CategorySpec a -> Either ExpandError (Lexeme Expanded a)
expandLexeme cs (Grapheme (GMulti g))
    | Just (g', '~') <- unsnoc g
        = Right $ Grapheme $ GMulti g'
    | otherwise = Right $
        case lookup g cs of
            Just c -> Category c
            Nothing -> Grapheme (GMulti g)
  where
    -- taken from base-4.19
    unsnoc :: [a] -> Maybe ([a], a)
    unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
    {-# INLINABLE unsnoc #-}

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
    go cs (DefineCategory name val) = flip (M.insert name) cs <$> expand cs val
    go cs (DefineFeature spec) = do
        baseValues <- expand cs $ featureBaseValues spec
        derivedCats <- traverse (traverse $ expand cs) $ featureDerived spec

        baseValues' <- for (elements baseValues) $ \case
            Left (GMulti g) -> Right g
            _ -> Left InvalidBaseValue
        let baseLen = length baseValues'
            derivedValues = elements . snd <$> derivedCats
        unless (all ((==baseLen) . length) derivedValues) $
            Left MismatchedLengths

        let features = zipWith
               (\base ds -> (base, FromElements $ Left (GMulti base) : ds))
               baseValues'
               (transpose derivedValues)
            newCats =
                maybe [] (pure . (,baseValues)) (featureBaseName spec)
                ++ derivedCats
                ++ features
        Right $ foldl' (flip $ uncurry M.insert) cs newCats

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
