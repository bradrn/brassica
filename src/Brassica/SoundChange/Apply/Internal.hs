{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
-- Module      : Brassica.SoundChange.Apply.Internal
-- Copyright   : See LICENSE file
-- License     : BSD3
-- Maintainer  : Brad Neimann
--
-- __Warning:__ This module is __internal__, and does __not__ follow
-- the Package Versioning Policy. It may be useful for extending
-- Brassica, but be prepared to track development closely if you import
-- this module.
--
-- This module contains the lower-level functions used by Brassica to
-- match and apply sound changes. The overall algorithm is similar to
-- that described by [Howard (1973)](https://dspace.mit.edu/bitstream/handle/1721.1/12982/26083289-MIT.pdf?sequence=2).
--
-- Some essential points:
--
--     * Words are represented as 'MultiZipper's, with a cursor index
--       and zero or more tagged indices. A sound change can then be
--       applied ('applyRule') by advancing through the word from left
--       to right. (Right-to-left application is achieved by reversing
--       both word and rule.)
--
--     * For each potential application site, 'applyOnce' checks the
--       target, environments and exceptions. If they are all
--       satisfied, it then replaces the target graphemes with the
--       replacement graphemes. After running 'applyOnce',
--       'setupForNextApplication' can be used to advance to the next
--       application site.
--
--     * The lowest-level function for matching is 'match', which
--       matches an individual 'Lexeme' at some point in a word. The
--       lowest-level function for replacement is 'mkReplacement',
--       which constructs replacement graphemes.
module Brassica.SoundChange.Apply.Internal
       (
       -- * Lexeme matching
         RuleTag(..)
       , RuleStatus(..)
       , MatchOutput(..)
       , FeatureState(..)
       , newOutput
       , initialOutput
       , match
       , matchMany
       , mkReplacement
       , exceptionAppliesAtPoint
       , matchRuleAtPoint
       -- * Sound change application
       , applyOnce
       , setupForNextApplication
       , applyRule
       , checkGraphemes
       , applyStatement
       , applyRuleStr
       , applyStatementStr
       , applyChanges
       -- * Logging
       , LogItem(..)
       , PWordLog(..)
       , toPWordLog
       , reportAsHtmlRows
       , reportAsText
       , applyStatementWithLog
       , applyChangesWithLog
       , applyChangesWithLogs
       , applyChangesWithChanges
       , applyChangesWithReports
       , applyChangesWithChangesAndReports
       ) where

import Control.Applicative ((<|>))
import Control.Category ((>>>))
import Control.Monad ((>=>), (<=<), join)  -- needed for mtl>=2.3
import Data.Containers.ListUtils (nubOrd)
import Data.Functor ((<&>))
import Data.List (elemIndex)
import Data.Maybe (maybeToList, fromMaybe, listToMaybe, mapMaybe)
import GHC.Generics (Generic)

import Control.DeepSeq (NFData)
import Control.Monad.State

import qualified Data.Map.Strict as Map

import Brassica.SoundChange.Apply.Internal.MultiZipper
import Brassica.SoundChange.Types
import Data.Bifunctor (Bifunctor(first))

-- | Defines the tags used when applying a 'Rule'.
data RuleTag
    = AppStart     -- ^ The start of a rule application
    | TargetStart  -- ^ The start of the target
    | TargetEnd    -- ^ The end of the target
    | PrevEnd
    -- ^ The end of the replacement from the last rule application
    -- (used to avoid infinite loops from iterative rules)
    deriving (Eq, Ord, Show)

-- | A monad in which to process a 'MultiZipper' over
-- 'Char's. Essentially a @StateT (MultiZipper RuleTag Grapheme) []@:
-- it stores the 'MultiZipper' as state, and allows failure,
-- backtracking and multiple answers (backtracking over the state
-- too).
newtype RuleAp a = RuleAp { runRuleAp :: MultiZipper RuleTag Grapheme -> [(a, MultiZipper RuleTag Grapheme)] }
    deriving (Functor, Applicative, Monad, MonadState (MultiZipper RuleTag Grapheme), MonadFail)
      via (StateT (MultiZipper RuleTag Grapheme) [])

-- | Lift a partial modification function into a 'State'. Update state
-- if it succeeds, otherwise rollback.
modifyMay :: Monad m => (s -> Maybe s) -> StateT s m ()
modifyMay f = modify $ \s -> fromMaybe s (f s)

-- | Monadic version of 'modify'.
modifyM :: Monad m => (s -> m s) -> StateT s m ()
modifyM f = StateT (fmap ((),) . f)

-- | Given a nondeterministic stateful function, lift it into a
-- 'StateT' computation which returns 'Nothing' when the original
-- action would have failed with no results.
try :: (s -> [(a, s)]) -> StateT s [] (Maybe a)
try p = StateT $ \s ->
    case p s of
        [] -> [(Nothing, s)]
        r -> first Just <$> r

-- | The result of matching a 'Feature' or 'Autosegment': either a
-- specific index in the 'Feature', or an indeterminate result (when
-- no indices matched)
data FeatureState = Index Int | Indeterminate
    deriving (Show, Eq)

-- | Describes the output of a 'match' operation.
data MatchOutput = MatchOutput
    { -- | For each non-backreferenced category matched: the index of
      -- the matched grapheme in that category.
      matchedCatIxs    :: [Int]
      -- | For each optional group: whether it matched or not
    , matchedOptionals :: [Bool]
      -- | For each wildcard: the graphemes which it matched
    , matchedWildcards :: [[Grapheme]]
      -- | For each Kleene star: how many repetitions it matched
    , matchedKleenes   :: [Int]
      -- | The actual graphemes which were matched
    , matchedGraphemes :: [Grapheme]
      -- | The features which were matched, by name
    , matchedFeatures :: Map.Map String [FeatureState]
      -- | Backreferenced categories which were matched, by ID
    , matchedBackrefIds :: Map.Map String Int
      -- | Backreferenced features which were matched, by ID
    , matchedFeatureIds :: Map.Map String FeatureState
    } deriving (Show)

-- | Create 'MatchOutput' for next section of rule given last output
-- (preserving backreferences but emptying all other fields)
newOutput :: MatchOutput -> MatchOutput
newOutput m = MatchOutput
    { matchedCatIxs = []
    , matchedOptionals = []
    , matchedWildcards = []
    , matchedKleenes = []
    , matchedGraphemes = []
    , matchedFeatures = Map.empty
    , matchedBackrefIds = matchedBackrefIds m
    , matchedFeatureIds = matchedFeatureIds m
    }

-- | The empty 'MatchOutput'
initialOutput :: MatchOutput
initialOutput = MatchOutput [] [] [] [] [] Map.empty Map.empty Map.empty

modifyMatchedGraphemes :: ([Grapheme] -> [Grapheme]) -> MatchOutput -> MatchOutput
modifyMatchedGraphemes f MatchOutput{..} = MatchOutput{matchedGraphemes=f matchedGraphemes, ..}

appendGrapheme :: MatchOutput -> Grapheme -> MatchOutput
appendGrapheme out g = modifyMatchedGraphemes (++[g]) out


zipWith' :: [a] -> [b] -> (a -> b -> c) -> [c]
zipWith' xs ys f = zipWith f xs ys

-- Note: see c37afd7028afd4f610d8701799fb6857e2f9b3d9
-- for motivation for the below functions

insertAt :: Int -> a -> [a] -> [a]
insertAt n a as = let (xs,ys) = splitAt n as in xs ++ (a:ys)

insertAtOptional :: Int -> Bool -> MatchOutput -> MatchOutput
insertAtOptional n o mz = mz { matchedOptionals = insertAt n o $ matchedOptionals mz }

insertAtCat :: Int -> Int -> MatchOutput -> MatchOutput
insertAtCat n i mz = mz { matchedCatIxs = insertAt n i $ matchedCatIxs mz }

insertAtKleene :: Int -> Int -> MatchOutput -> MatchOutput
insertAtKleene n i mz = mz { matchedKleenes = insertAt n i $ matchedKleenes mz }

appendFeatureAt :: Int -> String -> FeatureState -> MatchOutput -> MatchOutput
appendFeatureAt n name fs out = out { matchedFeatures = Map.alter go name $ matchedFeatures out }
  where
    go Nothing = Just [fs]
    go (Just fss) = Just $ insertAt n fs fss

-- | Match a single 'Lexeme' against a 'MultiZipper', and advance the
-- 'MultiZipper' past the match. For each match found, returns the
-- updated 'MatchOutput' tupled with the updated 'MultiZipper'.
match :: MatchOutput          -- ^ The previous 'MatchOutput'
      -> Maybe Grapheme       -- ^ The previously-matched grapheme, if any. (Used to match a 'Geminate'.)
      -> Lexeme Expanded 'Matched  -- ^ The lexeme to match.
      -> MultiZipper t Grapheme   -- ^ The 'MultiZipper' to match against.
      -> [(MatchOutput, MultiZipper t Grapheme)]
      -- ^ The output: a tuple @(g, mz)@ as described below.
match out prev (Optional l) mz =
    let i = length (matchedOptionals out)
    in
        (insertAtOptional i False out, mz) :
        matchMany (insertAtOptional i True out) prev l mz
match out prev (GreedyOptional l) mz =
    let i = length (matchedOptionals out)
        m = matchMany (insertAtOptional i True out) prev l mz
    in case m of
        -- skip, but only if no matches
        [] -> [(insertAtOptional i False out, mz)]
        _ -> m
match out prev (Wildcard l) mz = matchWildcard out prev l mz
match out prev (Kleene l) mz = matchKleene out prev l mz
match out _ (Grapheme g) mz = (appendGrapheme out g,) <$> maybeToList (matchGrapheme g mz)
match out prev (Category (FromElements gs)) mz =
    concat $ zipWith' gs [0..] $ \ls i ->
        -- make sure to insert new index BEFORE any new ones which
        -- might be added by the recursive call
        first (insertAtCat (length $ matchedCatIxs out) i) <$>
            matchMany out prev ls mz
match out prev (GreedyCategory c) mz =
    -- Take first match only
    case match out prev (Category c) mz of
        [] -> []
        (m:_) -> [m]
match out prev Geminate mz = case prev of
    Nothing -> []
    Just prev' -> (appendGrapheme out prev',) <$> maybeToList (matchGrapheme prev' mz)
match out prev (Backreference (Left ident) (FromElements gs)) mz
    | Nothing <- Map.lookup ident (matchedBackrefIds out) =
        -- first occurrence, set backref
        -- similar to Category case above
        concat $ zipWith' gs [0..] $ \ls i ->
            first (\o -> o { matchedBackrefIds = Map.insert ident i $ matchedBackrefIds o })
                <$> matchMany out prev ls mz
match out prev (Backreference i (FromElements gs)) mz = do
    ls <- maybeToList $ case i of
        Left i' -> (gs !?) =<< Map.lookup i' (matchedBackrefIds out)
        Right i' -> (gs !?) =<< matchedCatIxs out !? (i'-1)
    matchMany out prev ls mz
match out prev (Feature r _n (Just ident) kvs l) mz
    | Just fs <- Map.lookup ident (matchedFeatureIds out) = do
        -- similar to next case, but just check that features are the same
        -- (NB. feature name is irrelevant for this)
        (out', mz') <- match out prev l mz
        let fs' = case matchedGraphemes out' of
                gs | Just g <- lastMay gs -> checkFeature kvs g
                _ -> Indeterminate
            satisfied = case (fs, fs') of
                (Indeterminate, _) -> True
                (_, Indeterminate) -> True
                _  ->
                    if r
                    then fs /= fs'  -- reverse comparison
                    else fs == fs'
        if satisfied
            then pure (out', mz')
            else []
match out prev (Feature _r n ident kvs l) mz = do
    let i = maybe 0 length $ Map.lookup n (matchedFeatures out)
    (out', mz') <- match out prev l mz
    let fs = case matchedGraphemes out' of
            gs | Just g <- lastMay gs -> checkFeature kvs g
            _ -> Indeterminate
    pure $ case ident of
        Nothing -> (appendFeatureAt i n fs out', mz')
        Just ident' ->
            ( out' { matchedFeatureIds = Map.insert ident' fs $ matchedFeatureIds out' }
            , mz'
            )
match out prev (Autosegment n kvs gs) mz =
    -- act as 'Category' + 'Feature', without capture
    gs >>= \a -> match out prev (Feature False n Nothing kvs $ Grapheme a) mz

checkFeature :: Eq a => [[a]] -> a -> FeatureState
checkFeature [] _ = Indeterminate
checkFeature (gs:gss) x
    | Just i <- x `elemIndex` gs = Index i
    | otherwise = checkFeature gss x

matchKleene
    :: MatchOutput
    -> Maybe Grapheme
    -> Lexeme Expanded 'Matched
    -> MultiZipper t Grapheme
    -> [(MatchOutput, MultiZipper t Grapheme)]
matchKleene origOut = go 0 origOut
  where
    go !n out prev l mz = case match out prev l mz of
        [] -> [
            ( insertAtKleene (length $ matchedKleenes origOut) n out
            , mz
            ) ]
        r -> r >>= \(out', mz') -> go (n+1) out' prev l mz'

matchWildcard
    :: MatchOutput
    -> Maybe Grapheme
    -> Lexeme Expanded 'Matched
    -> MultiZipper t Grapheme
    -> [(MatchOutput, MultiZipper t Grapheme)]
matchWildcard = go []
  where
    go matched out prev l mz = case match out prev l mz of
        [] -> maybeToList (consume mz) >>= \case
            ("#", _) -> []   -- don't continue past word boundary
            (g, mz') -> go (g:matched) (appendGrapheme out g) prev l mz'
        r -> r <&> \(out', mz') ->
            ( out'
              { matchedWildcards = matchedWildcards out' ++ [reverse matched]
              }
            , mz'
            )

matchGrapheme :: Grapheme -> MultiZipper t Grapheme -> Maybe (MultiZipper t Grapheme)
matchGrapheme g = matchGraphemeP (==g)

matchGraphemeP :: (Grapheme -> Bool) -> MultiZipper t Grapheme -> Maybe (MultiZipper t Grapheme)
matchGraphemeP p mz = value mz >>= \cs -> if p cs then fwd mz else Nothing

-- | Match a list of several 'Lexeme's against a
-- 'MultiZipper'. Arguments and output are as with 'match'.
matchMany :: MatchOutput
          -> Maybe Grapheme
          -> [Lexeme Expanded 'Matched]
          -> MultiZipper t Grapheme
          -> [(MatchOutput, MultiZipper t Grapheme)]
matchMany out _ [] mz = [(out, mz)]
matchMany out prev (l:ls) mz =
    match out prev l mz >>= \(out', mz') ->
    matchMany  out' (lastMay (matchedGraphemes out') <|> prev) ls mz'

-- Small utility function, not exported
lastMay :: [a] -> Maybe a
lastMay l = if null l then Nothing else Just (last l)

data ReplacementIndices = ReplacementIndices
    { ixInCategories :: Int
    , ixInOptionals :: Int
    , ixInWildcards :: Int
    , ixInKleenes :: Int
    , ixInFeatures :: Map.Map String Int
    , forcedCategory :: Maybe CategoryNumber
    } deriving (Show)

data CategoryNumber = CategoryNumber Int | CategoryId String | Nondeterministic
    deriving (Show)

advanceCategory :: ReplacementIndices -> Int -> (CategoryNumber, ReplacementIndices)
advanceCategory ix cslen =
    case forcedCategory ix of
        Just i -> (i, ix { forcedCategory = Nothing })
        Nothing ->
            let i = ixInCategories ix in
                ( if i < cslen then CategoryNumber i else Nondeterministic
                , ix { ixInCategories = i+1 }
                )

advanceOptional :: ReplacementIndices -> (Int, ReplacementIndices)
advanceOptional ix =
    let i = ixInOptionals ix
    in (i, ix { ixInOptionals = i+1 })

advanceWildcard :: ReplacementIndices -> (Int, ReplacementIndices)
advanceWildcard ix =
    let i = ixInWildcards ix
    in (i, ix { ixInWildcards = i+1 })

advanceKleene :: ReplacementIndices -> (Int, ReplacementIndices)
advanceKleene ix =
    let i = ixInKleenes ix
    in (i, ix { ixInKleenes = i+1 })

advanceFeature :: String -> ReplacementIndices -> Maybe (Int, ReplacementIndices)
advanceFeature n ix =
    case Map.lookup n (ixInFeatures ix) of
        Nothing -> Just (0, ix { ixInFeatures = Map.insert n 1    $ ixInFeatures ix })
        Just i  -> Just (i, ix { ixInFeatures = Map.adjust (+1) n $ ixInFeatures ix })

forceCategory :: CategoryNumber -> ReplacementIndices -> ReplacementIndices
forceCategory i ixs = ixs { forcedCategory = Just i }

-- | Partially safe list indexing
(!?) :: [a] -> Int -> Maybe a
(x:_ ) !? 0 = Just x
(_:xs) !? n = xs !? (n-1)
[]     !? _ = Nothing

-- | Given a list of 'Lexeme's specifying a replacement, generate all
-- possible replacements and apply them to the given input.
mkReplacement
    :: MatchOutput              -- ^ The result of matching against the target
    -> [Lexeme Expanded 'Replacement]    -- ^ The 'Lexeme's specifying the replacement.
    -> MultiZipper t Grapheme
    -> [MultiZipper t Grapheme]
mkReplacement out = \ls -> fmap (fst . snd) . go startIxs ls . (,Nothing)
  where
    startIxs = ReplacementIndices 0 0 0 0 Map.empty Nothing

    go
        :: ReplacementIndices
        -> [Lexeme Expanded 'Replacement]
        -> (MultiZipper t Grapheme, Maybe Grapheme)
        -> [(ReplacementIndices, (MultiZipper t Grapheme, Maybe Grapheme))]
    go ixs []     (mz, prev) = [(ixs, (mz, prev))]
    go ixs (l:ls) (mz, prev) = do
        (ixs', (mz', prev')) <- replaceLex ixs l mz prev
        go ixs' ls (mz', prev')

    numCatsMatched = length $ matchedCatIxs out

    replaceLex
        :: ReplacementIndices
        -> Lexeme Expanded 'Replacement
        -> MultiZipper t Grapheme
        -> Maybe Grapheme
        -> [(ReplacementIndices, (MultiZipper t Grapheme, Maybe Grapheme))]
    replaceLex ixs (Grapheme g) mz _prev = [(ixs, (insert g mz, Just g))]
    replaceLex ixs (Category (FromElements gs)) mz prev =
        case advanceCategory ixs numCatsMatched of
            (CategoryNumber ci, ixs') ->
                case matchedCatIxs out !? ci of
                    Just i | Just g' <- gs !? i ->
                        case g' of
                            [Grapheme g] -> [(ixs', (insert g mz, Just g))]
                            ls -> go ixs' ls (mz, prev)
                    _ -> [(ixs', (insert "\xfffd" mz, Nothing))]  -- Unicode replacement character
            (CategoryId ci, ixs') ->  -- as above
                case Map.lookup ci (matchedBackrefIds out) of
                    Just i | Just g' <- gs !? i ->
                        case g' of
                            [Grapheme g] -> [(ixs', (insert g mz, Just g))]
                            ls -> go ixs' ls (mz, prev)
                    _ -> [(ixs', (insert "\xfffd" mz, Nothing))]  -- Unicode replacement character
            (Nondeterministic, ixs') -> gs >>= \case
                [Grapheme g] -> [(ixs', (insert g mz, Just g))]
                ls -> go ixs' ls (mz, prev)
    replaceLex ixs (Optional ls) mz prev =
        let (co, ixs') = advanceOptional ixs in
            case matchedOptionals out !? co of
                Just True -> go ixs' ls (mz, prev)
                Just False -> [(ixs', (mz, Nothing))]
                Nothing    ->  (ixs', (mz, Nothing)) : go ixs ls (mz, prev)
    replaceLex ixs Metathesis mz _prev =
        [( ixs
         , ( flip insertMany mz $ reverse $ matchedGraphemes out
           , listToMaybe $ matchedGraphemes out)
         )]
    replaceLex ixs Geminate mz prev =
        [(ixs, (flip insertMany mz $ maybeToList prev, prev))]
    replaceLex ixs Discard mz prev =
        let (_, ixs') = advanceCategory ixs numCatsMatched
        in [(ixs', (mz, prev))]
    replaceLex ixs (Backreference (Left i) c) mz prev =
        let ixs' = forceCategory (CategoryId i) ixs
        in replaceLex ixs' (Category c) mz prev
    replaceLex ixs (Backreference (Right i) c) mz prev =
        let ixs' = forceCategory (CategoryNumber $ i-1) ixs -- 1-based indexing!
        in replaceLex ixs' (Category c) mz prev
    replaceLex ixs (Multiple c) mz prev =
        let ixs' = forceCategory Nondeterministic ixs
        in replaceLex ixs' (Category c) mz prev
    replaceLex ixs (Wildcard l) mz prev =
        let (i, ixs') = advanceWildcard ixs
        in case matchedWildcards out !? i of
            Just w -> go ixs' (fmap Grapheme w ++ [l]) (mz, prev)
            -- need to add 'l' here too
            Nothing -> replaceLex ixs' l mz prev
    replaceLex ixs (Kleene l) mz prev =
        let (i, ixs') = advanceKleene ixs
        in case matchedKleenes out !? i of
            Just n -> go ixs' (replicate n l) (mz, prev)
            Nothing -> [(ixs', (mz, prev))]
    replaceLex ixs (Feature r n ident kvs l) mz prev =
        let (fs, ixs') = case ident of
                Nothing -> case advanceFeature n ixs of
                    Just (i, ixs_)
                        | Just fss <- Map.lookup n (matchedFeatures out)
                        , Just fs_ <- fss !? i
                        -> (fs_, ixs_)
                    _ -> (Indeterminate, ixs)
                Just ident' -> case Map.lookup ident' (matchedFeatureIds out) of
                    Just fs_ -> (fs_, ixs)
                    Nothing -> (Indeterminate, ixs)
        in do
            (ixs'', (mz', prev')) <- replaceLex ixs' l mz prev
            case (kvs, prev') of
                (gs:_, Just g) | g /= "#" -> do
                    g' <- case fs of
                        Index i -> applyFeature kvs g <$>
                            if r
                            then filter (/=i) [0 .. length gs - 1]
                            else pure i
                        Indeterminate -> applyFeature kvs g <$> [0 .. length gs - 1]
                    -- now overwrite previous grapheme
                    let mz'' = zap (Just . const g') mz'
                    pure (ixs'', (mz'', Just g'))
                -- cannot modify nonexistent or boundary grapheme,
                -- or if there are zero key-value pairs
                _ -> pure (ixs'', (mz', prev'))
    replaceLex ixs (Autosegment _ _ []) mz prev = pure (ixs, (mz, prev))
    replaceLex ixs (Autosegment n kvs (g:_)) mz prev =
        -- ignore other segments, just produce a single one
        -- as modulated by a 'Feature'
        replaceLex ixs (Feature False n Nothing kvs $ Grapheme g) mz prev

applyFeature :: [[String]] -> String -> Int -> String
applyFeature [] g _ = g
applyFeature (gs:gss) g i
    | g `elem` gs = fromMaybe "\xfffd" $ gs !? i
    | otherwise = applyFeature gss g i

-- | Given a 'Rule' and a 'MultiZipper', determines whether the
-- 'exception' of that rule (if any) applies starting at the current
-- position of the 'MultiZipper'; if it does, returns the index at
-- which each matching target begins.
exceptionAppliesAtPoint
    :: [Lexeme Expanded 'Matched]  -- ^ Target
    -> Environment Expanded        -- ^ Exceptional environment
    -> MultiZipper RuleTag Grapheme -> [Int]
exceptionAppliesAtPoint target (ex1, ex2) mz = fmap fst $ flip runRuleAp mz $ do
    ex1Out <- RuleAp $ matchMany initialOutput Nothing ex1
    pos <- gets curPos
    targetOut@MatchOutput{matchedGraphemes} <- RuleAp $ matchMany (newOutput ex1Out) Nothing target
    _ <- RuleAp $ matchMany (newOutput targetOut) (listToMaybe matchedGraphemes) ex2
    return pos

-- | Given a target and environment, determine if the rule matches at
-- the current position of the 'MultiZipper'. If so, for each match,
-- return the 'MatchOutput' and the output 'MultiZipper'. The output
-- 'MultiZipper' is advanced past the matched environment, and has its
-- 'RuleTag's set as appropriate.
matchRuleAtPoint
    :: [Lexeme Expanded 'Matched]  -- ^ Target
    -> Environment Expanded        -- ^ Environment
    -> MultiZipper RuleTag Grapheme
    -> [(MatchOutput, MultiZipper RuleTag Grapheme)]
matchRuleAtPoint target (env1,env2) mz = flip runRuleAp mz $ do
    let initMO = MatchOutput [] [] [] [] [] Map.empty Map.empty Map.empty
    env1Out <- RuleAp $ matchMany initMO Nothing env1
    -- start of target needs to be INSIDE 'MultiZipper'!
    -- otherwise get weird things like /x/#_ resulting in
    -- #abc#→#xabd#x when it should be #abc#→#xabc#
    gets atBoundary >>= \case
        True -> RuleAp $ const []
        False -> do
            modify $ tag TargetStart
            matchResult <- RuleAp $ matchMany (newOutput env1Out) Nothing target
            modify $ tag TargetEnd
            env2Out <- RuleAp $ matchMany (newOutput matchResult)
                (listToMaybe $ matchedGraphemes matchResult) env2
            -- environment can affect replacement via IDs
            -- only, so collect those
            return matchResult
                { matchedFeatureIds = matchedFeatureIds env2Out
                , matchedBackrefIds = matchedBackrefIds env2Out
                }

-- | Status of a rule application at a single location.
data RuleStatus
    = SuccessNormal      -- ^ Rule was successful, with no need for special handling
    | SuccessEpenthesis  -- ^ Rule was successful, but cursor was not advanced (need to avoid infinite loop)
    | Failure            -- ^ Rule failed
    deriving (Eq, Show)

-- | Given a 'Rule', determine if the rule matches at the current
-- point; if so, apply the rule, adding appropriate tags.
applyOnce :: Rule Expanded -> StateT (MultiZipper RuleTag Grapheme) [] RuleStatus
applyOnce Rule{..} =
    modify (tag AppStart) >> go environment
  where
    go [] = return Failure
    go (env:envs) = do
        result <- try (matchRuleAtPoint target env)
        case result of
            Just out -> do
                exs <- case exception of
                    Nothing -> pure []
                    Just ex -> gets $ join . toList .
                        extend' (exceptionAppliesAtPoint target ex)
                originalWord <- get
                let pMay = locationOf TargetStart originalWord
                    pMay' = locationOf PrevEnd originalWord
                case pMay of
                    Nothing -> error "applyOnce: start of target was not tagged"
                    Just p
                        | p `elem` exs -> return Failure
                        -- do not apply rule if it would be
                        -- applied twice to the same substring
                        | Just p' <- pMay', p < p' -> return Failure
                        | otherwise -> do
                        modifyMay $ delete (TargetStart, TargetEnd)
                        modifyMay $ seek TargetStart
                        modifyM $ \w ->
                            let replacedWords = mkReplacement out replacement w
                            in case sporadic flags of
                                -- make sure to re-insert original word
                                PerApplication -> originalWord : replacedWords
                                _ -> replacedWords
                        -- we want TargetEnd to move forward as the replacement is added,
                        -- but not TargetStart, so restore its old position
                        modifyMay $ tagAt TargetStart p
                        return $
                            -- An epenthesis rule will cause an infinite loop
                            -- if it matched no graphemes before the replacement
                            if null (matchedGraphemes out) && null (fst env)
                                then SuccessEpenthesis
                                else SuccessNormal
            Nothing -> modifyMay (seek AppStart) >> go envs

-- | Remove tags and advance the current index to the next t'Grapheme'
-- after the rule application.
setupForNextApplication
    :: RuleStatus
    -> Rule Expanded
    -> MultiZipper RuleTag Grapheme
    -> Maybe (MultiZipper RuleTag Grapheme)
setupForNextApplication status Rule{flags=Flags{nonOverlappingTarget}} =
    resetTags <=< case status of
        SuccessNormal ->
            seek (if nonOverlappingTarget then TargetEnd else TargetStart)
        SuccessEpenthesis ->
            -- need to move forward if applying an epenthesis rule to avoid an infinite loop
            seek TargetEnd >=> fwd
        Failure -> seek AppStart >=> fwd
  where
    resetTags mz =
        -- update PrevEnd to farthest replaced position on success,
        -- or keep it the same on failure
        let p = locationOf TargetEnd mz
            p' = locationOf PrevEnd mz
            newPrevEnd = case status of
                Failure -> p'
                _ -> max p p'
        in maybe Just (tagAt PrevEnd) newPrevEnd $ untag mz

-- | Apply a 'Rule' to a 'MultiZipper'. The application will start at
-- the beginning of the 'MultiZipper', and will be repeated as many
-- times as possible. Returns all valid results.
--
-- Note: unlike 'applyRuleStr', this can produce duplicate outputs.
applyRule :: Rule Expanded -> MultiZipper RuleTag Grapheme -> [MultiZipper RuleTag Grapheme]
applyRule r = \mz ->    -- use a lambda so mz isn't shadowed in the where block
    let result = case applyDirection (flags r) of
            LTR -> repeatRule $ toBeginning mz
            -- Apply RTL by reversing both rule and word
            RTL -> fmap reverseMZ $ repeatRule $ toBeginning $ reverseMZ mz
    in case sporadic (flags r) of
        PerWord -> mz : result
        _ -> result  -- PerApplication handled in 'applyOnce'
  where
    r' = case applyDirection (flags r) of
        LTR -> r
        RTL -> Rule
            { target = reverse $ target r
            , replacement = reverse $ replacement r
            , environment = reverseEnv <$> environment r
            , exception = reverseEnv <$> exception r
            , flags = flags r
            , plaintext = plaintext r
            }

    reverseEnv (e1, e2) = (reverse e2, reverse e1)

    repeatRule
        :: MultiZipper RuleTag Grapheme
        -> [MultiZipper RuleTag Grapheme]
    repeatRule mz = runStateT (applyOnce r') mz >>= \(status, mz') ->
        if (status /= Failure) && applyOnceOnly (flags r')
        then [mz']
        else maybe [mz'] repeatRule (setupForNextApplication status r' mz')

-- | Check if a 'MultiZipper' matches a 'Filter'.
filterMatches :: Filter Expanded -> MultiZipper RuleTag Grapheme -> Bool
filterMatches (Filter _ ls) = go . toBeginning
  where
    go mz =
        let mzs = matchMany initialOutput Nothing ls mz
        in case mzs of
            [] -> maybe False go $ fwd mz  -- try next position if there is one
            _ -> True  -- filter has matched

-- | Check that the 'MultiZipper' contains only graphemes listed in
-- the given list, replacing all unlisted graphemes other than @"#"@
-- with U+FFFD.
checkGraphemes :: [Grapheme] -> MultiZipper RuleTag Grapheme -> MultiZipper RuleTag Grapheme
checkGraphemes gs = fmap $ \case
    "#" -> "#"
    g -> if g `elem` gs then g else "\xfffd"

-- | Apply a 'Statement' to a 'MultiZipper', returning zero, one or
-- more results.
applyStatement
    :: Statement Expanded GraphemeList
    -> MultiZipper RuleTag Grapheme
    -> [MultiZipper RuleTag Grapheme]
applyStatement (RuleS r) mz = applyRule r mz
applyStatement (FilterS f) mz
    | filterMatches f mz = []
    | otherwise = [mz]
applyStatement ReportS mz = [mz]
applyStatement (DirectiveS (GraphemeList noreplace gs)) mz
    | noreplace = [mz]
    | otherwise = [checkGraphemes gs mz]

-- | Apply a single sound change 'Rule' to a word.
applyRuleStr :: Rule Expanded -> PWord -> [PWord]
-- Note: 'fromJust' is safe here as 'apply' should always succeed
applyRuleStr r =
    addBoundaries
    >>> fromListStart
    >>> applyRule r
    >>> fmap (toList >>> removeBoundaries)
    >>> nubOrd

-- | Apply a single 'Statement' to a word. The statement can be a
-- sound change, a filter, or any other element which remains in a
-- sound change file after expansion.
applyStatementStr :: Statement Expanded GraphemeList -> PWord -> [PWord]
applyStatementStr st =
    addBoundaries
    >>> fromListStart
    >>> applyStatement st
    >>> fmap (toList >>> removeBoundaries)
    >>> nubOrd

-- | A log item representing a single action. When this action was a
-- sound change, Specifies the action which was applied, as well as
-- the ‘before’ and ‘after’ states.
data LogItem r
    = ActionApplied r PWord (Maybe PWord)
    | ReportWord PWord
    deriving (Show, Functor, Generic, NFData)

-- action :: LogItem r -> Maybe r
-- action (ActionApplied r _ _) = Just r
-- action (ReportWord _) = Nothing

logInput :: LogItem r -> PWord
logInput (ActionApplied _ i _) = i
logInput (ReportWord i) = i

logOutput :: LogItem r -> Maybe PWord
logOutput (ActionApplied _ _ o) = o
logOutput (ReportWord o) = Just o

-- | Logs the evolution of a word as it undergoes sound changes and
-- other actions. The actions are of type @r@ (which will usually be
-- 'Statement').
data PWordLog r = PWordLog
    { initialWord :: PWord
    -- ^ The initial word, before any actions have been applied
    , derivations :: [(Maybe PWord, r)]
    -- ^ The state of the word after each action @r@, stored alongside
    -- the action which was applied at each point; or 'Nothing' if the
    -- word was deleted
    } deriving (Show, Functor, Generic, NFData)

-- | Convert a list of individual 'LogItem's for a single word to a
-- 'PWordLog' summarising the whole evolution of that word.
toPWordLog :: [LogItem r] -> Maybe (PWordLog r)
toPWordLog [] = Nothing
toPWordLog ls@(l : _) = Just $ PWordLog
    { initialWord = logInput l
    , derivations = flip mapMaybe ls $ \case
            ActionApplied action _ output -> Just (output, action)
            _ -> Nothing
    }

-- | Pretty-print a single 'PWordLog' as rows of an HTML table. For
-- instance, the example log given in the documentation for
-- 'reportAsText' would be converted to the following HTML:
--
-- > <tr><td>tara</td><td>&rarr;</td><td>tazha</td><td>(r / zh)</td></tr><tr><td></td><td>&rarr;</td><td>tazh</td><td>(V / / _ #)</td></tr>
--
-- Which might be displayed in a browser as follows:
--
-- +------+---+-------+-------------+
-- | tara | → | tazha | (r / zh)    |
-- +------+---+-------+-------------+
-- |      | → | tazh  | (V / / _ #) |
-- +------+---+-------+-------------+

reportAsHtmlRows
    :: (r -> String)  -- ^ Specifies how to pretty-print actions as text
    -> PWordLog r -> String
reportAsHtmlRows render item = go (concatWithBoundary $ initialWord item) (derivations item)
  where
    go _ [] = ""
    go cell1 ((output, action) : ds) =
        ("<tr><td>" ++ cell1 ++ "</td><td>&rarr;</td><td>"
         ++ maybe "<i>deleted</i>" concatWithBoundary output
         ++ "</td><td>(" ++ render action ++ ")</td></tr>")
        ++ go "" ds

-- | Pretty-print a 'PWordLog' as plain text. For instance, this log:
--
-- @
-- 'PWordLog'
--   { 'initialWord' = ["t", "a", "r", "a"]
--   , 'derivations' =
--     [ ('Just' ["t", "a", "zh", "a"], "r \/ zh")
--     , ('Just' ["t", "a", "zh"], "V \/ \/ _ #")
--     ]
--   }
-- @
--
-- Would be pretty-printed by @'reportAsText' 'id'@ as:
--
-- > tara
-- >   -> tazha  (r / zh)
-- >   -> tazh   (V / / _ #)
reportAsText
    :: (r -> String)  -- ^ Specifies how to pretty-print actions as text
    -> PWordLog r -> String
reportAsText render item = unlines $
    concatWithBoundary (initialWord item) : fmap toLine (alignWithPadding $ derivations item)
  where
    alignWithPadding :: [(Maybe PWord, b)] -> [([Char], b)]
    alignWithPadding ds =
        let (rawOutputs, actions) = unzip ds
            outputs = maybe "(deleted)" concatWithBoundary <$> rawOutputs
            maxlen = maximum $ length <$> outputs
            padded = outputs <&> \o -> o ++ replicate (maxlen - length o) ' '
        in zip padded actions

    toLine (output, action) = "  -> " ++ output ++ "  (" ++ render action ++ ")"

-- | Apply a single 'Statement' to a word. Returns a 'LogItem' for
-- each possible result, or @[]@ if the rule does not apply and the
-- input is returned unmodified.
applyStatementWithLog
    :: Statement Expanded GraphemeList
    -> PWord
    -> [LogItem (Statement Expanded GraphemeList)]
applyStatementWithLog ReportS w = [ReportWord w]
applyStatementWithLog st w = case applyStatementStr st w of
    [] -> [ActionApplied st w Nothing]
    [w'] | w' == w -> []
    r -> ActionApplied st w . Just <$> r

-- | Apply 'SoundChanges' to a word. For each possible result, returns
-- a 'LogItem' for each 'Statement' which altered the input, plus a
-- 'ReportWord' for at least the input and output words.
applyChangesWithLog
    :: SoundChanges Expanded GraphemeList
    -> PWord
    -> [[LogItem (Statement Expanded GraphemeList)]]
applyChangesWithLog [] w = [[ReportWord w]]  -- always report the final result
applyChangesWithLog scs w = (ReportWord w:) <$> go scs w
  where
    go [] w' = [[ReportWord w']]  -- alw'ays report the final result
    go (st:sts) w' =
        case applyStatementWithLog st w' of
            [] -> go sts w'
            outputActions -> outputActions >>= \case
                l@(ReportWord w'') -> (l :) <$> go sts w''
                l@(ActionApplied _ _ output) -> case output of
                    Just w'' -> (l :) <$> go sts w''
                    -- apply no further changes to a deleted w'ord
                    Nothing -> [[l]]

-- | Apply a set of 'SoundChanges' to a word, returning a log of which
-- sound changes applied to produce each output word.
applyChangesWithLogs
    :: SoundChanges Expanded GraphemeList
    -> PWord
    -> [PWordLog (Statement Expanded GraphemeList)]
applyChangesWithLogs scs w = mapMaybe toPWordLog $ applyChangesWithLog  scs w

-- | Apply a set of 'SoundChanges' to a word, returning the final
-- output word(s).
applyChanges :: SoundChanges Expanded GraphemeList -> PWord -> [PWord]
applyChanges sts w =
    mapMaybe lastOutput $ applyChangesWithLog sts w
  where
    -- If no changes were applied, output is same as input
    lastOutput [] = Just w
    lastOutput ls = logOutput $ last ls

-- | Apply a set of 'SoundChanges' to a word, returning the final
-- output word(s) as well as any intermediate results from 'ReportS'.
applyChangesWithReports :: SoundChanges Expanded GraphemeList -> PWord -> [[PWord]]
applyChangesWithReports sts w = getReports <$> applyChangesWithLog sts w
  where
    getReports [] = []
    getReports [ActionApplied _ _ (Just w')] = [w']
    getReports (ReportWord w':ls) = w' : getReports ls
    getReports (_:ls) = getReports ls

-- | Apply a set of 'SoundChanges' to a word returning the final
-- output word(s), as well as a boolean value indicating whether each
-- has been changed from the input (accounting for 'highlightChanges'
-- flags).
applyChangesWithChanges :: SoundChanges Expanded GraphemeList -> PWord -> [(Maybe PWord, Bool)]
applyChangesWithChanges sts w = applyChangesWithLog sts w <&> \case
    [] -> (Just w, False)
    logs -> (logOutput $ last logs, hasChanged logs)
  where
    hasChanged = any $ \case
        ActionApplied (RuleS rule) _ _ -> highlightChanges $ flags rule
        ActionApplied (FilterS _) _ _ -> False  -- cannot highlight nonexistent word
        ActionApplied (DirectiveS _) _ _ -> True
        ActionApplied ReportS _ _ -> False  -- reporting a word yields no change
        ReportWord _ -> False

-- | Apply a set of 'SoundChanges' to a word, returning the final
-- output word(s) as well as any intermediate results from 'ReportS',
-- each with a boolean marking changed results (as with 'applyChangesWithChanges').
applyChangesWithChangesAndReports :: SoundChanges Expanded GraphemeList -> PWord -> [[(PWord, Bool)]]
applyChangesWithChangesAndReports sts w = getReports <$> applyChangesWithLog sts w
  where
    getReports :: [LogItem (Statement Expanded GraphemeList)] -> [(PWord, Bool)]
    getReports [] = []
    getReports l = go False l
      where
        go _ [] = []
        go hasChanged (ActionApplied action _ _:ls) =
            let hasChanged' = case action of
                    RuleS rule -> hasChanged || highlightChanges (flags rule)
                    _ -> hasChanged
            in go hasChanged' ls
        go hasChanged (ReportWord w':ls) = (w', hasChanged) : go hasChanged ls
