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
{-# LANGUAGE ViewPatterns          #-}

{-| __Warning:__ This module is __internal__, and does __not__ follow
  the Package Versioning Policy. It may be useful for extending
  Brassica, but be prepared to track development closely if you import
  this module.
-}
module Brassica.SoundChange.Apply.Internal
       ( -- * Types
         RuleTag(..)
       -- * Lexeme matching
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
import Data.Containers.ListUtils (nubOrd)
import Data.Function ((&))
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
#if __GLASGOW_HASKELL__ > 806
    , MonadFail
#endif
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
      -- | The graphemes which were matched
    , matchedGraphemes :: [Grapheme]
    } deriving (Show)

modifyMatchedGraphemes :: ([Grapheme] -> [Grapheme]) -> MatchOutput -> MatchOutput
modifyMatchedGraphemes f MatchOutput{..} = MatchOutput{matchedGraphemes=f matchedGraphemes, ..}

appendGrapheme :: MatchOutput -> Grapheme -> MatchOutput
appendGrapheme out g = modifyMatchedGraphemes (++[g]) out

instance Semigroup MatchOutput where
    (MatchOutput a1 b1 c1) <> (MatchOutput a2 b2 c2) = MatchOutput (a1++a2) (b1++b2) (c1++c2)

-- | Match a single 'Lexeme' against a 'MultiZipper', and advance the
-- 'MultiZipper' past the match. For each match found, returns the
-- 'MatchOutput' tupled with the updated 'MultiZipper'.
match :: OneOf a 'Target 'Env
      => MatchOutput          -- ^ The previous 'MatchOutput'
      -> Maybe Grapheme       -- ^ The previously-matched grapheme, if any. (Used to match a 'Geminate'.)
      -> Lexeme a             -- ^ The lexeme to match.
      -> MultiZipper t Grapheme   -- ^ The 'MultiZipper' to match against.
      -> [(MatchOutput, MultiZipper t Grapheme)]
      -- ^ The output: a tuple @(g, mz)@ as described below.
match out prev (Optional l) mz =
    (out <> MatchOutput [] [False] [], mz) :
    matchMany (out <> MatchOutput [] [True] []) prev l mz
match out prev w@(Wildcard l) mz = case match out prev l mz of
    [] -> maybeToList (consume mz) >>= \case
        (GBoundary, _) -> []   -- don't continue past word boundary
        (g, mz') -> match (appendGrapheme out g) prev w mz'
    r -> r
match out prev k@(Kleene l) mz = case match out prev l mz of
    [] -> [(MatchOutput [] [] [], mz)]
    r -> r >>= \(out', mz') -> case match out' prev k mz' of
        [] -> error "match: Kleene should never fail"
        r' -> r'
match out _ (Grapheme g) mz = (out <> MatchOutput [] [] [g],) <$> maybeToList (matchGrapheme g mz)
match out _ (Category gs) mz =
    gs
    -- Attempt to match each option in category...
    & fmap (matchCategoryEl mz)
    -- ...get the index of each match...
    & zipWith (\i m -> fmap (i,) m) [0..]
    -- ...and take all matches
    & (>>= maybeToList)
    & fmap (\(i, (g, mz')) -> (out <> MatchOutput [i] [] g, mz'))
match out prev Geminate mz = case prev of
    Nothing -> []
    Just prev' -> (out <> MatchOutput [] [] [prev'],) <$> maybeToList (matchGrapheme prev' mz)
match out _prev (Backreference i gs) mz = maybeToList $ do
    catIx <- matchedCatIxs out !? (i-1)
    g <- gs !? catIx
    (matched, mz') <- matchCategoryEl mz g
    pure (modifyMatchedGraphemes (++matched) out, mz')

matchCategoryEl :: MultiZipper t Grapheme -> Grapheme -> Maybe ([Grapheme], MultiZipper t Grapheme)
matchCategoryEl mz g = ([g],) <$> matchGrapheme g mz

matchGrapheme :: Grapheme -> MultiZipper t Grapheme -> Maybe (MultiZipper t Grapheme)
matchGrapheme g = matchGraphemeP (==g)

matchGraphemeP :: (Grapheme -> Bool) -> MultiZipper t Grapheme -> Maybe (MultiZipper t Grapheme)
matchGraphemeP p mz = value mz >>= \cs -> if p cs then fwd mz else Nothing

-- | Match a list of several 'Lexeme's against a
-- 'MultiZipper'. Arguments and output are the same as with 'match',
-- though the outputs are given as a list of indices and graphemes
-- rather than as a single index and grapheme.
matchMany :: OneOf a 'Target 'Env
          => MatchOutput
          -> Maybe Grapheme
          -> [Lexeme a]
          -> MultiZipper t Grapheme
          -> [(MatchOutput, MultiZipper t Grapheme)]
matchMany out _ [] mz = [(out, mz)]
matchMany out prev (l:ls) mz =
    match out prev l mz >>= \(out', mz') ->
    matchMany  out' (lastMay (matchedGraphemes out') <|> prev) ls mz'

-- | 'matchMany' without any previous match output.
matchMany' :: OneOf a 'Target 'Env
          => Maybe Grapheme
          -> [Lexeme a]
          -> MultiZipper t Grapheme
          -> [(MatchOutput, MultiZipper t Grapheme)]
matchMany' = matchMany (MatchOutput [] [] [])

-- Small utility function, not exported
lastMay :: [a] -> Maybe a
lastMay l = if null l then Nothing else Just (last l)

data ReplacementIndices = ReplacementIndices
    { ixInCategories :: Int
    , ixInOptionals :: Int
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
    -> [Lexeme 'Replacement]    -- ^ The 'Lexeme's specifying the replacement.
    -> MultiZipper t Grapheme
    -> [MultiZipper t Grapheme]
mkReplacement out = \ls -> fmap (fst . snd) . go startIxs ls . (,Nothing)
  where
    startIxs = ReplacementIndices 0 0 Nothing

    go
        :: ReplacementIndices
        -> [Lexeme 'Replacement]
        -> (MultiZipper t Grapheme, Maybe Grapheme)
        -> [(ReplacementIndices, (MultiZipper t Grapheme, Maybe Grapheme))]
    go ixs []     (mz, prev) = [(ixs, (mz, prev))]
    go ixs (l:ls) (mz, prev) = do
        (ixs', (mz', prev')) <- replaceLex ixs l mz prev
        go ixs' ls (mz', prev')

    numCatsMatched = length $ matchedCatIxs out

    replaceLex
        :: ReplacementIndices
        -> Lexeme 'Replacement
        -> MultiZipper t Grapheme
        -> Maybe Grapheme
        -> [(ReplacementIndices, (MultiZipper t Grapheme, Maybe Grapheme))]
    replaceLex ixs (Grapheme g) mz _prev = [(ixs, (insert g mz, Just g))]
    replaceLex ixs (Category gs) mz _prev =
        case advanceCategory ixs numCatsMatched of
            (CategoryNumber ci, ixs') ->
                let i = matchedCatIxs out !! ci in
                    case gs !? i of
                        Just g   -> [(ixs', (insert g mz, Just g))]
                        Nothing  -> [(ixs', (insert (GMulti "\xfffd") mz, Nothing))]  -- Unicode replacement character
            (Nondeterministic, ixs') -> gs <&> \g -> (ixs', (insert g mz, Just g))
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

-- | Given a 'Rule' and a 'MultiZipper', determines whether the
-- 'exception' of that rule (if any) applies starting at the current
-- position of the 'MultiZipper'; if it does, returns the index of the
-- first element of each matching 'target'.
exceptionAppliesAtPoint :: [Lexeme 'Target] -> Environment -> MultiZipper RuleTag Grapheme -> [Int]
exceptionAppliesAtPoint target (ex1, ex2) mz = fmap fst $ flip runRuleAp mz $ do
    _ <- RuleAp $ matchMany' Nothing ex1
    pos <- gets curPos
    MatchOutput{matchedGraphemes} <- RuleAp $ matchMany' Nothing target
    _ <- RuleAp $ matchMany' (listToMaybe matchedGraphemes) ex2
    return pos

-- | Given a target and environment, determine if they rule
-- matches. If so, for each match, set the appropriate 'RuleTag's and
-- return a tuple of @(is, gs)@, where @gs@ is a list of matched
-- t'Grapheme's, and @is@ is a list of indices, one for each
-- 'Category' lexeme matched.
matchRuleAtPoint
    :: [Lexeme 'Target]
    -> Environment
    -> MultiZipper RuleTag Grapheme
    -> [(MatchOutput, MultiZipper RuleTag Grapheme)]
matchRuleAtPoint target (env1,env2) mz = flip runRuleAp mz $ do
    _ <- RuleAp $ matchMany' Nothing env1
    -- start of target needs to be INSIDE 'MultiZipper'!
    -- otherwise get weird things like /x/#_ resulting in
    -- #abc#→#xabd#x when it should be #abc#→#xabc#
    gets atBoundary >>= \case
        True -> RuleAp $ const []
        False -> do
            modify $ tag TargetStart
            matchResult <- RuleAp $ matchMany' Nothing target
            modify $ tag TargetEnd
            _ <- RuleAp $ matchMany' (listToMaybe $ matchedGraphemes matchResult) env2
            return matchResult

-- | Given a 'Rule', determine if the rule matches at the current
-- point; if so, apply the rule, adding appropriate tags.
applyOnce :: Rule -> StateT (MultiZipper RuleTag Grapheme) [] Bool
applyOnce r@Rule{target, replacement, exception} =
    modify (tag AppStart) >> go (environment r)
  where
    go [] = return False
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
                    then return False
                    else do
                        modifyMay $ modifyBetween (TargetStart, TargetEnd) $ const []
                        modifyMay $ seek TargetStart
                        modifyM $ mkReplacement out replacement
                        return True
            Nothing -> modifyMay (seek AppStart) >> go envs

-- | Remove tags and advance the current index to the next t'Grapheme'
-- after the rule application.
setupForNextApplication :: Bool -> Rule -> MultiZipper RuleTag Grapheme -> Maybe (MultiZipper RuleTag Grapheme)
setupForNextApplication success r@Rule{flags=Flags{applyDirection}} =
    fmap untag . case applyDirection of
        RTL -> seek AppStart >=> bwd
        LTR ->
            if success
            then
                if null (target r)
                then -- need to move forward if applying an epenthesis rule to avoid an infinite loop
                    seek TargetEnd >=> fwd
                else seek TargetEnd
            else seek AppStart >=> fwd

-- | Apply a 'Rule' to a 'MultiZipper'. The application will start at
-- the beginning of the 'MultiZipper', and will be repeated as many
-- times as possible. Returns all valid results.
applyRule :: Rule -> MultiZipper RuleTag Grapheme -> [MultiZipper RuleTag Grapheme]
applyRule r = \mz ->    -- use a lambda so mz isn't shadowed in the where block
    let startingPos = case applyDirection $ flags r of
            LTR -> toBeginning mz
            RTL -> toEnd mz
        result = repeatRule (applyOnce r) startingPos
    in if sporadic $ flags r
          then mz : result
          else result
  where
    repeatRule
        :: StateT (MultiZipper RuleTag Grapheme) [] Bool
        -> MultiZipper RuleTag Grapheme
        -> [MultiZipper RuleTag Grapheme]
    repeatRule m mz = runStateT m mz >>= \(success, mz') ->
        if success && applyOnceOnly (flags r)
        then [mz']
        else case setupForNextApplication success r mz' of
            Just mz'' -> repeatRule m mz''
            Nothing -> [mz']

-- | Check that the 'MultiZipper' contains only graphemes listed in
-- the given 'CategoriesDecl', replacing all unlisted graphemes with
-- U+FFFD.
checkGraphemes :: CategoriesDecl -> MultiZipper RuleTag Grapheme -> MultiZipper RuleTag Grapheme
checkGraphemes (CategoriesDecl gs) = fmap $ \case
    GBoundary -> GBoundary
    g -> if g `elem` gs then g else GMulti "\xfffd"

-- | Apply a 'Statement' to a 'MultiZipper'. This is a simple wrapper
-- around 'applyRule' and 'checkGraphemes'.
applyStatement :: Statement -> MultiZipper RuleTag Grapheme -> [MultiZipper RuleTag Grapheme]
applyStatement (RuleS r) mz = applyRule r mz
applyStatement (CategoriesDeclS gs) mz = [checkGraphemes gs mz]

-- | Apply a single 'Rule' to a word.
--
-- Note: duplicate outputs from this function are removed. To keep
-- duplicates, use the lower-level internal function 'applyRule'
-- directly.
applyRuleStr :: Rule -> PWord -> [PWord]
-- Note: 'fromJust' is safe here as 'apply' should always succeed
applyRuleStr r s = nubOrd $ fmap toList $ applyRule r $ fromListStart s

-- | Apply a single 'Statement' to a word.
--
-- Note: as with 'applyRuleStr', duplicate outputs from this function
-- are removed. To keep duplicates, use the lower-level internal
-- function 'applyStatement' directly.
applyStatementStr :: Statement -> PWord -> [PWord]
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
    , output :: PWord
    } deriving (Show, Functor, Generic, NFData)

-- | Logs the evolution of a 'PWord' as various actions are applied to
-- it. The actions (usually 'Statement's) are of type @r@.
data PWordLog r = PWordLog
    { initialWord :: PWord
    -- ^ The initial word, before any actions have been applied
    , derivations :: [(PWord, r)]
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
         ++ concatWithBoundary output
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
    alignWithPadding ds =
        let (fmap concatWithBoundary -> outputs, actions) = unzip ds
            maxlen = maximum $ length <$> outputs
            padded = outputs <&> \o -> o ++ replicate (maxlen - length o) ' '
        in zip padded actions

    toLine (output, action) = "  -> " ++ output ++ "  (" ++ render action ++ ")"

-- | Apply a single 'Statement' to a word. Returns a 'LogItem' for
-- each possible result, or @[]@ if the rule does not apply and the
-- input is returned unmodified.
applyStatementWithLog :: Statement -> PWord -> [LogItem Statement]
applyStatementWithLog st w = case applyStatementStr st w of
    [w'] -> if w' == w then [] else [ActionApplied st w w']
    r -> ActionApplied st w <$> r

-- | Apply 'SoundChanges' to a word. For each possible result, returns
-- a 'LogItem' for each 'Statement' which altered the input.
applyChangesWithLog :: SoundChanges -> PWord -> [[LogItem Statement]]
applyChangesWithLog [] _ = [[]]
applyChangesWithLog (st:sts) w =
    case applyStatementWithLog st w of
        [] -> applyChangesWithLog sts w
        items -> items >>= \l@ActionApplied{output=w'} ->
            (l :) <$> applyChangesWithLog sts w'

-- | Apply 'SoundChanges' to a word, returning an 'PWordLog'
-- for each possible result.
applyChangesWithLogs :: SoundChanges -> PWord -> [PWordLog Statement]
applyChangesWithLogs scs w = mapMaybe toPWordLog $ applyChangesWithLog  scs w

-- | Apply a set of 'SoundChanges' to a word.
applyChanges :: SoundChanges -> PWord -> [PWord]
applyChanges sts w =
    lastOutput <$> applyChangesWithLog sts w
  where
    lastOutput [] = w
    lastOutput ls = output $ last ls

-- | Apply 'SoundChanges' to a word returning the final results, as
-- well as a boolean value indicating whether the word should be
-- highlighted in a UI due to changes from its initial value. (Note
-- that this accounts for 'highlightChanges' values.)
applyChangesWithChanges :: SoundChanges -> PWord -> [(PWord, Bool)]
applyChangesWithChanges sts w = applyChangesWithLog sts w <&> \case
    [] -> (w, False)
    logs -> (output $ last logs, hasChanged logs)
  where
    hasChanged = any $ \case
        ActionApplied{action=RuleS rule} -> highlightChanges $ flags rule
        ActionApplied{action=CategoriesDeclS _} -> True
