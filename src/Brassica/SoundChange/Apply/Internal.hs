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

{-| __Warning:__ This module is __internal__, and does __not__ follow
  the Package Versioning Policy. It may be useful for extending
  Brassica, but be prepared to track development closely if you import
  this module.
-}
module Brassica.SoundChange.Apply.Internal
       (
       -- * Lexeme matching
         RuleTag(..)
       , RuleStatus(..)
       , MatchOutput(..)
       , match
       , matchMany
       , matchMany'
       , mkReplacement
       , exceptionAppliesAtPoint
       , matchRuleAtPoint
       -- * Sound change application
       , applyOnce
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
       ) where

import Control.Applicative ((<|>))
import Control.Category ((>>>))
import Control.Monad ((>=>), join)  -- needed for mtl>=2.3
import Data.Containers.ListUtils (nubOrd)
import Data.Functor ((<&>))
import Data.Maybe (maybeToList, fromMaybe, listToMaybe, mapMaybe)
import GHC.Generics (Generic)

import Control.DeepSeq (NFData)
import Control.Monad.State

import Brassica.SoundChange.Apply.Internal.MultiZipper
import Brassica.SoundChange.Types
import Data.Bifunctor (Bifunctor(first))

-- | Defines the tags used when applying a 'Rule'.
data RuleTag
    = AppStart     -- ^ The start of a rule application
    | TargetStart  -- ^ The start of the target
    | TargetEnd    -- ^ The end of the target
    deriving (Eq, Ord, Show)

-- | A monad in which to process a 'MultiZipper' over
-- 'Char's. Essentially a @StateT (MultiZipper RuleTag Grapheme) []@:
-- it stores the 'MultiZipper' as state, and allows failure,
-- backtracking and multiple answers (backtracking over the state
-- too).
newtype RuleAp a = RuleAp { runRuleAp :: MultiZipper RuleTag Grapheme -> [(a, MultiZipper RuleTag Grapheme)] }
    deriving (Functor, Applicative, Monad, MonadState (MultiZipper RuleTag Grapheme)

    , MonadFail

    )
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

-- | Describes the output of a 'match' operation.
data MatchOutput = MatchOutput
    { -- | For each category matched, the index of the matched
      -- grapheme in that category.
      matchedCatIxs    :: [Int]
      -- | For each optional group whether it matched or not
    , matchedOptionals :: [Bool]
      -- | For each wildcard, the graphemes which it matched
    , matchedWildcards :: [[Grapheme]]
      -- | For each Kleene star, how many repititions it matched
    , matchedKleenes   :: [Int]
      -- | The graphemes which were matched
    , matchedGraphemes :: [Grapheme]
    } deriving (Show)

modifyMatchedGraphemes :: ([Grapheme] -> [Grapheme]) -> MatchOutput -> MatchOutput
modifyMatchedGraphemes f MatchOutput{..} = MatchOutput{matchedGraphemes=f matchedGraphemes, ..}

appendGrapheme :: MatchOutput -> Grapheme -> MatchOutput
appendGrapheme out g = modifyMatchedGraphemes (++[g]) out

instance Semigroup MatchOutput where
    (MatchOutput a1 b1 c1 d1 e1) <> (MatchOutput a2 b2 c2 d2 e2) =
        MatchOutput (a1++a2) (b1++b2) (c1++c2) (d1++d2) (e1++e2)

zipWith' :: [a] -> [b] -> (a -> b -> c) -> [c]
zipWith' xs ys f = zipWith f xs ys

-- Note: see c37afd7028afd4f610d8701799fb6857e2f9b3d9
-- for motivation for the below functions

insertAt :: Int -> a -> [a] -> [a]
insertAt n a as = let (xs,ys) = splitAt n as in xs ++ (a:ys)

insertAtCat :: Int -> Int -> MatchOutput -> MatchOutput
insertAtCat n i mz = mz { matchedCatIxs = insertAt n i $ matchedCatIxs mz }

insertAtKleene :: Int -> Int -> MatchOutput -> MatchOutput
insertAtKleene n i mz = mz { matchedKleenes = insertAt n i $ matchedKleenes mz }

-- | Match a single 'Lexeme' against a 'MultiZipper', and advance the
-- 'MultiZipper' past the match. For each match found, returns the
-- 'MatchOutput' tupled with the updated 'MultiZipper'.
match :: MatchOutput          -- ^ The previous 'MatchOutput'
      -> Maybe Grapheme       -- ^ The previously-matched grapheme, if any. (Used to match a 'Geminate'.)
      -> Lexeme Expanded 'Matched  -- ^ The lexeme to match.
      -> MultiZipper t Grapheme   -- ^ The 'MultiZipper' to match against.
      -> [(MatchOutput, MultiZipper t Grapheme)]
      -- ^ The output: a tuple @(g, mz)@ as described below.
match out prev (Optional l) mz =
    (out <> MatchOutput [] [False] [] [] [], mz) :
    matchMany (out <> MatchOutput [] [True] [] [] []) prev l mz
match out prev (Wildcard l) mz = matchWildcard out prev l mz
match out prev (Kleene l) mz = matchKleene out prev l mz
match out _ (Grapheme g) mz = (out <> MatchOutput [] [] [] [] [g],) <$> maybeToList (matchGrapheme g mz)
match out prev (Category (FromElements gs)) mz =
    concat $ zipWith' gs [0..] $ \e i ->
        -- make sure to insert new index BEFORE any new ones which
        -- might be added by the recursive call
        first (insertAtCat (length $ matchedCatIxs out) i) <$>
            case e of
                Left  g  -> match out prev (Grapheme g :: Lexeme Expanded a) mz
                Right ls -> matchMany out prev ls mz
match out prev Geminate mz = case prev of
    Nothing -> []
    Just prev' -> (out <> MatchOutput [] [] [] [] [prev'],) <$> maybeToList (matchGrapheme prev' mz)
match out prev (Backreference i (FromElements gs)) mz = do
    e <- maybeToList $
        (gs !?) =<< matchedCatIxs out !? (i-1)
    case e of
        Left  g  -> match out prev (Grapheme g :: Lexeme Expanded a) mz
        Right ls -> matchMany out prev ls mz

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
            (GBoundary, _) -> []   -- don't continue past word boundary
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
-- 'MultiZipper'. Arguments and output are the same as with 'match',
-- though the outputs are given as a list of indices and graphemes
-- rather than as a single index and grapheme.
matchMany :: MatchOutput
          -> Maybe Grapheme
          -> [Lexeme Expanded 'Matched]
          -> MultiZipper t Grapheme
          -> [(MatchOutput, MultiZipper t Grapheme)]
matchMany out _ [] mz = [(out, mz)]
matchMany out prev (l:ls) mz =
    match out prev l mz >>= \(out', mz') ->
    matchMany  out' (lastMay (matchedGraphemes out') <|> prev) ls mz'

-- | 'matchMany' without any previous match output.
matchMany' :: Maybe Grapheme
          -> [Lexeme Expanded 'Matched]
          -> MultiZipper t Grapheme
          -> [(MatchOutput, MultiZipper t Grapheme)]
matchMany' = matchMany (MatchOutput [] [] [] [] [])

-- Small utility function, not exported
lastMay :: [a] -> Maybe a
lastMay l = if null l then Nothing else Just (last l)

data ReplacementIndices = ReplacementIndices
    { ixInCategories :: Int
    , ixInOptionals :: Int
    , ixInWildcards :: Int
    , ixInKleenes :: Int
    , forcedCategory :: Maybe CategoryNumber
    } deriving (Show)

data CategoryNumber = CategoryNumber Int | Nondeterministic
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
    startIxs = ReplacementIndices 0 0 0 0 Nothing

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
                            Left g -> [(ixs', (insert g mz, Just g))]
                            Right ls -> go ixs' ls (mz, prev)
                    _ -> [(ixs', (insert (GMulti "\xfffd") mz, Nothing))]  -- Unicode replacement character
            (Nondeterministic, ixs') -> gs >>= \case
                Left g -> [(ixs', (insert g mz, Just g))]
                Right ls -> go ixs' ls (mz, prev)
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
    replaceLex ixs (Backreference i c) mz prev =
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

-- | Given a 'Rule' and a 'MultiZipper', determines whether the
-- 'exception' of that rule (if any) applies starting at the current
-- position of the 'MultiZipper'; if it does, returns the index of the
-- first element of each matching 'target'.
exceptionAppliesAtPoint
    :: [Lexeme Expanded 'Matched]
    -> Environment Expanded
    -> MultiZipper RuleTag Grapheme -> [Int]
exceptionAppliesAtPoint target (ex1, ex2) mz = fmap fst $ flip runRuleAp mz $ do
    ex1Out <- RuleAp $ matchMany' Nothing ex1
    pos <- gets curPos
    MatchOutput{matchedGraphemes} <- RuleAp $ matchMany' Nothing target
    _ <- RuleAp $ matchMany ex1Out (listToMaybe matchedGraphemes) ex2
    return pos

-- | Given a target and environment, determine if they rule
-- matches. If so, for each match, set the appropriate 'RuleTag's and
-- return a tuple of @(is, gs)@, where @gs@ is a list of matched
-- t'Grapheme's, and @is@ is a list of indices, one for each
-- 'Category' lexeme matched.
matchRuleAtPoint
    :: [Lexeme Expanded 'Matched]
    -> Environment Expanded
    -> MultiZipper RuleTag Grapheme
    -> [(MatchOutput, MultiZipper RuleTag Grapheme)]
matchRuleAtPoint target (env1,env2) mz = flip runRuleAp mz $ do
    env1Out <- RuleAp $ matchMany' Nothing env1
    -- start of target needs to be INSIDE 'MultiZipper'!
    -- otherwise get weird things like /x/#_ resulting in
    -- #abc#→#xabd#x when it should be #abc#→#xabc#
    gets atBoundary >>= \case
        True -> RuleAp $ const []
        False -> do
            modify $ tag TargetStart
            matchResult <- RuleAp $ matchMany' Nothing target
            modify $ tag TargetEnd
            _ <- RuleAp $ matchMany env1Out (listToMaybe $ matchedGraphemes matchResult) env2
            return matchResult

data RuleStatus
    = SuccessNormal      -- ^ Rule was successful, no need for special handling
    | SuccessEpenthesis  -- ^ Rule was successful, but cursor was not advanced: need to avoid infinite loop
    | Failure            -- ^ Rule failed
    deriving (Eq, Show)

-- | Given a 'Rule', determine if the rule matches at the current
-- point; if so, apply the rule, adding appropriate tags.
applyOnce :: Rule Expanded -> StateT (MultiZipper RuleTag Grapheme) [] RuleStatus
applyOnce r@Rule{target, replacement, exception} =
    modify (tag AppStart) >> go (environment r)
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
                gets (locationOf TargetStart) >>= \p ->
                    if maybe True (`elem` exs) p
                    then return Failure
                    else do
                        originalWord <- get
                        modifyMay $ delete (TargetStart, TargetEnd)
                        modifyMay $ seek TargetStart
                        modifyM $ \w ->
                            let replacedWords = mkReplacement out replacement w
                            in case sporadic (flags r) of
                                -- make sure to re-insert original word
                                PerApplication -> originalWord : replacedWords
                                _ -> replacedWords
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
setupForNextApplication status Rule{flags=Flags{applyDirection}} =
    fmap untag . case applyDirection of
        RTL -> seek AppStart >=> bwd
        LTR -> case status of
            SuccessNormal -> seek TargetEnd
            SuccessEpenthesis ->
                -- need to move forward if applying an epenthesis rule to avoid an infinite loop
                seek TargetEnd >=> fwd
            Failure -> seek AppStart >=> fwd

-- | Apply a 'Rule' to a 'MultiZipper'. The application will start at
-- the beginning of the 'MultiZipper', and will be repeated as many
-- times as possible. Returns all valid results.
applyRule :: Rule Expanded -> MultiZipper RuleTag Grapheme -> [MultiZipper RuleTag Grapheme]
applyRule r = \mz ->    -- use a lambda so mz isn't shadowed in the where block
    let startingPos = case applyDirection $ flags r of
            LTR -> toBeginning mz
            RTL -> toEnd mz
        result = repeatRule (applyOnce r) startingPos
    in case sporadic (flags r) of
        PerWord -> mz : result
        _ -> result  -- PerApplication handled in 'applyOnce'
  where
    repeatRule
        :: StateT (MultiZipper RuleTag Grapheme) [] RuleStatus
        -> MultiZipper RuleTag Grapheme
        -> [MultiZipper RuleTag Grapheme]
    repeatRule m mz = runStateT m mz >>= \(status, mz') ->
        if (status /= Failure) && applyOnceOnly (flags r)
        then [mz']
        else case setupForNextApplication status r mz' of
            Just mz'' -> repeatRule m mz''
            Nothing -> [mz']

-- | Check if a 'MultiZipper' matches a 'Filter'.
filterMatches :: Filter Expanded -> MultiZipper RuleTag Grapheme -> Bool
filterMatches (Filter _ ls) = go . toBeginning
  where
    go mz =
        let mzs = matchMany' Nothing ls mz
        in case mzs of
            [] -> maybe False go $ fwd mz  -- try next position if there is one
            _ -> True  -- filter has matched

-- | Check that the 'MultiZipper' contains only graphemes listed in
-- the given 'CategoriesDecl', replacing all unlisted graphemes with
-- U+FFFD.
checkGraphemes :: [Grapheme] -> MultiZipper RuleTag Grapheme -> MultiZipper RuleTag Grapheme
checkGraphemes gs = fmap $ \case
    GBoundary -> GBoundary
    g -> if g `elem` gs then g else GMulti "\xfffd"

-- | Apply a 'Statement' to a 'MultiZipper', returning zero, one or
-- more results.
applyStatement
    :: Statement Expanded [Grapheme]
    -> MultiZipper RuleTag Grapheme
    -> [MultiZipper RuleTag Grapheme]
applyStatement (RuleS r) mz = applyRule r mz
applyStatement (FilterS f) mz
    | filterMatches f mz = []
    | otherwise = [mz]
applyStatement (DirectiveS gs) mz = [checkGraphemes gs mz]

-- | Apply a single 'Rule' to a word.
--
-- Note: duplicate outputs from this function are removed. To keep
-- duplicates, use the lower-level internal function 'applyRule'
-- directly.
applyRuleStr :: Rule Expanded -> PWord -> [PWord]
-- Note: 'fromJust' is safe here as 'apply' should always succeed
applyRuleStr r =
    addBoundaries
    >>> fromListStart
    >>> applyRule r
    >>> fmap (toList >>> removeBoundaries)
    >>> nubOrd

-- | Apply a single 'Statement' to a word.
--
-- Note: as with 'applyRuleStr', duplicate outputs from this function
-- are removed. To keep duplicates, use the lower-level internal
-- function 'applyStatement' directly.
applyStatementStr :: Statement Expanded [Grapheme] -> PWord -> [PWord]
applyStatementStr st =
    addBoundaries
    >>> fromListStart
    >>> applyStatement st
    >>> fmap (toList >>> removeBoundaries)
    >>> nubOrd

-- | A log item representing a single application of an action. (In
-- practise this will usually be a 'Statement'.) Specifies the action
-- which was applied, as well as the ‘before’ and ‘after’ states.
data LogItem r = ActionApplied
    { action :: r
    , input :: PWord
    , output :: Maybe PWord
    } deriving (Show, Functor, Generic, NFData)

-- | Logs the evolution of a 'PWord' as various actions are applied to
-- it. The actions (usually 'Statement's) are of type @r@.
data PWordLog r = PWordLog
    { initialWord :: PWord
    -- ^ The initial word, before any actions have been applied
    , derivations :: [(Maybe PWord, r)]
    -- ^ The state of the word after each action @r@, stored alongside
    -- the action which was applied at each point
    } deriving (Show, Functor, Generic, NFData)

toPWordLog :: [LogItem r] -> Maybe (PWordLog r)
toPWordLog [] = Nothing
toPWordLog ls@(l : _) = Just $ PWordLog
    { initialWord = input l
    , derivations = (\ActionApplied{..} -> (output, action)) <$> ls
    }

-- | Render a single 'PWordLog' to rows of an HTML table. For
-- instance, the example log given in the documentation for
-- 'reportAsText' would be converted to the following HTML:
--
-- > "<tr><td>tara</td><td>&rarr;</td><td>tazha</td><td>(r / zh)</td></tr><tr><td></td><td>&rarr;</td><td>tazh</td><td>(V / / _ #)</td></tr>"
--
-- Which might be displayed in an HTML table as something like the
-- following:
--
-- +------+---+-------+-------------+ 
-- | tara | → | tazha | (r / zh)    |
-- +------+---+-------+-------------+ 
-- |      | → | tazh  | (V / / _ #) |
-- +------+---+-------+-------------+ 

reportAsHtmlRows :: (r -> String) -> PWordLog r -> String
reportAsHtmlRows render item = go (concatWithBoundary $ initialWord item) (derivations item)
  where
    go _ [] = ""
    go cell1 ((output, action) : ds) =
        ("<tr><td>" ++ cell1 ++ "</td><td>&rarr;</td><td>"
         ++ maybe "<i>deleted</i>" concatWithBoundary output
         ++ "</td><td>(" ++ render action ++ ")</td></tr>")
        ++ go "" ds

-- | Render a single 'PWordLog' to plain text. For instance, this log:
--
-- > PWordLog
-- >   { initialWord = ["t", "a", "r", "a"]
-- >   , derivations =
-- >     [ (["t", "a", "zh", "a"], "r / zh")
-- >     , (["t", "a", "zh"], "V / / _ #")
-- >     ]
-- >   }
--
-- Would render as:
--
-- > tara
-- >   -> tazha  (r / zh)
-- >   -> tazh   (V / / _ #)
reportAsText :: (r -> String) -> PWordLog r -> String
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
    :: Statement Expanded [Grapheme]
    -> PWord
    -> [LogItem (Statement Expanded [Grapheme])]
applyStatementWithLog st w = case applyStatementStr st w of
    [] -> [ActionApplied st w Nothing]
    [w'] | w' == w -> []
    r -> ActionApplied st w . Just <$> r

-- | Apply 'SoundChanges' to a word. For each possible result, returns
-- a 'LogItem' for each 'Statement' which altered the input.
applyChangesWithLog
    :: SoundChanges Expanded [Grapheme]
    -> PWord
    -> [[LogItem (Statement Expanded [Grapheme])]]
applyChangesWithLog [] _ = [[]]
applyChangesWithLog (st:sts) w =
    case applyStatementWithLog st w of
        [] -> applyChangesWithLog sts w
        outputActions -> outputActions >>= \l@ActionApplied{output} ->
            case output of
                Just w' -> (l :) <$> applyChangesWithLog sts w'
                -- apply no further changes to a deleted word
                Nothing -> [[l]]

-- | Apply 'SoundChanges' to a word, returning an 'PWordLog'
-- for each possible result.
applyChangesWithLogs
    :: SoundChanges Expanded [Grapheme]
    -> PWord
    -> [PWordLog (Statement Expanded [Grapheme])]
applyChangesWithLogs scs w = mapMaybe toPWordLog $ applyChangesWithLog  scs w

-- | Apply a set of 'SoundChanges' to a word.
applyChanges :: SoundChanges Expanded [Grapheme] -> PWord -> [PWord]
applyChanges sts w =
    mapMaybe lastOutput $ applyChangesWithLog sts w
  where
    -- If no changes were applied, output is same as input
    lastOutput [] = Just w
    lastOutput ls = output $ last ls

-- | Apply 'SoundChanges' to a word returning the final results, as
-- well as a boolean value indicating whether the word should be
-- highlighted in a UI due to changes from its initial value. (Note
-- that this accounts for 'highlightChanges' values.)
applyChangesWithChanges :: SoundChanges Expanded [Grapheme] -> PWord -> [(Maybe PWord, Bool)]
applyChangesWithChanges sts w = applyChangesWithLog sts w <&> \case
    [] -> (Just w, False)
    logs -> (output $ last logs, hasChanged logs)
  where
    hasChanged = any $ \case
        ActionApplied (RuleS rule) _ _ -> highlightChanges $ flags rule
        ActionApplied (FilterS _) _ _ -> False  -- cannot highlight nonexistent word
        ActionApplied (DirectiveS _) _ _ -> True
