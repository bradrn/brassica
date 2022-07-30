{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
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

module Brassica.SoundChange.Apply
       ( -- * Types
         RuleTag(..)
       -- * Lexeme matching
       , match
       , matchMany
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
       ) where

import Control.Applicative ((<|>))
import Data.Function ((&))
import Data.Maybe (maybeToList, fromMaybe, listToMaybe)

import Control.Monad.State

import Brassica.MultiZipper
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

prependGrapheme :: Grapheme -> MatchOutput -> MatchOutput
prependGrapheme g = modifyMatchedGraphemes (g:)

instance Semigroup MatchOutput where
    (MatchOutput a1 b1 c1) <> (MatchOutput a2 b2 c2) = MatchOutput (a1++a2) (b1++b2) (c1++c2)

-- | Match a single 'Lexeme' against a 'MultiZipper', and advance the
-- 'MultiZipper' past the match. For each match found, returns the
-- 'MatchOutput' tupled with the updated 'MultiZipper'.
match :: OneOf a 'Target 'Env
      => Maybe Grapheme       -- ^ The previously-matched grapheme, if any. (Used to match a 'Geminate'.)
      -> Lexeme a             -- ^ The lexeme to match.
      -> MultiZipper t Grapheme   -- ^ The 'MultiZipper' to match against.
      -> [(MatchOutput, MultiZipper t Grapheme)]
      -- ^ The output: a tuple @((i, g), mz)@ as described below.
match prev (Optional l) mz =
    (MatchOutput [] [False] [], mz) :
    (first (MatchOutput [] [True] [] <>) <$> matchMany prev l mz )
match prev w@(Wildcard l) mz = case match prev l mz of
    [] -> maybeToList (consume mz) >>= \(g, mz') ->
        first (prependGrapheme g) <$> match prev w mz'
    r -> r
match prev k@(Kleene l) mz = case match prev l mz of
    [] -> [(MatchOutput [] [] [], mz)]
    r -> r >>= \(out, mz') -> case match prev k mz' of
        [] -> error "match: Kleene should never fail"
        r' -> first (out <>) <$> r'
match _ (Grapheme g) mz = (MatchOutput [] [] [g],) <$> maybeToList (matchGrapheme g mz)
match _ (Category gs) mz =
    gs
    -- Attempt to match each option in category...
    & fmap (\case
               BoundaryEl -> if atBoundary mz then Just ([], mz) else Nothing
               GraphemeEl g -> ([g],) <$> matchGrapheme g mz)
    -- ...get the index of each match...
    & zipWith (\i m -> fmap (i,) m) [0..]
    -- ...and take all matches
    & (>>= maybeToList)
    & fmap (\(i, (g, mz')) -> (MatchOutput [i] [] g, mz'))
match _ Boundary mz = if atBoundary mz then [(MatchOutput [] [] [], mz)] else []
match prev Geminate mz = case prev of
    Nothing -> []
    Just prev' -> (MatchOutput [] [] [prev'],) <$> maybeToList (matchGrapheme prev' mz)

matchGrapheme :: Grapheme -> MultiZipper t Grapheme -> Maybe (MultiZipper t Grapheme)
matchGrapheme g = matchGraphemeP (==g)

matchGraphemeP :: (Grapheme -> Bool) -> MultiZipper t Grapheme -> Maybe (MultiZipper t Grapheme)
matchGraphemeP p mz = value mz >>= \cs -> if p cs then fwd mz else Nothing

-- | Match a list of several 'Lexeme's against a
-- 'MultiZipper'. Arguments and output are the same as with 'match',
-- though the outputs are given as a list of indices and graphemes
-- rather than as a single index and grapheme.
matchMany :: OneOf a 'Target 'Env
          => Maybe Grapheme
          -> [Lexeme a]
          -> MultiZipper t Grapheme
          -> [(MatchOutput, MultiZipper t Grapheme)]
matchMany = go (MatchOutput [] [] [])
  where
    go out _ [] mz = [(out, mz)]
    go out prev (l:ls) mz = match prev l mz >>= \(out', mz') ->
        go (out <> out') (lastMay (matchedGraphemes out') <|> prev) ls mz'

-- Small utility function, not exported
lastMay :: [a] -> Maybe a
lastMay l = if null l then Nothing else Just (last l)

-- | Given a list of 'Lexeme's specifying a replacement, generate the
-- replacement as a 'String'.
mkReplacement
    :: MatchOutput              -- ^ The result of matching against the target
    -> [Lexeme 'Replacement]    -- ^ The 'Lexeme's specifying the replacement.
    -> MultiZipper t Grapheme
    -> MultiZipper t Grapheme
mkReplacement = \out ls -> fst . snd . go out ls . (,Nothing)
  where
    go out []     (mz, prev) = (out, (mz, prev))
    go out (l:ls) (mz, prev) =
        let (out', (mz', prev')) = replaceLex out l mz prev
        in go out' ls (mz', prev')


    replaceLex
        :: MatchOutput
        -> Lexeme 'Replacement
        -> MultiZipper t Grapheme
        -> Maybe Grapheme
        -> (MatchOutput, (MultiZipper t Grapheme, Maybe Grapheme))
    replaceLex out (Grapheme g) mz _prev = (out, (insert g mz, Just g))
    replaceLex out@MatchOutput{matchedCatIxs=(i:is)} (Category gs) mz _prev = (out{matchedCatIxs=is},) $
        if i < length gs
        then case gs !! i of GraphemeEl g -> (insert g mz, Just g)
        else (insert "\xfffd" mz, Nothing)  -- Unicode replacement character
    replaceLex out@MatchOutput{matchedCatIxs=[]} (Category _) mz _prev = (out, (insert "\xfffd" mz, Nothing))
    replaceLex MatchOutput{matchedOptionals=(o:os), ..} (Optional ls) mz prev =
        let out' = MatchOutput{matchedOptionals=os, ..}
        in if o
           then go out' ls (mz, prev)
           else (out', (mz, Nothing))
    replaceLex out@MatchOutput{matchedOptionals=[]} (Optional _) mz _prev = (out, (insert "\xfffd" mz, Nothing))
    replaceLex out@MatchOutput{matchedGraphemes}     Metathesis  mz _prev =
        (out, (flip insertMany mz $ reverse matchedGraphemes, listToMaybe matchedGraphemes))
    replaceLex out                                   Geminate    mz prev =
        (out, (flip insertMany mz $ maybeToList prev, prev))
    replaceLex out@MatchOutput{matchedCatIxs=(_:is)} Discard mz prev = (out{matchedCatIxs=is}, (mz, prev))
    replaceLex out@MatchOutput{matchedCatIxs=[]} Discard mz prev = (out, (insert "\xfffd" mz, prev))

-- | Given a 'Rule' and a 'MultiZipper', determines whether the
-- 'exception' of that rule (if any) applies starting at the current
-- position of the 'MultiZipper'; if it does, returns the index of the
-- first element of each matching 'target'.
exceptionAppliesAtPoint :: [Lexeme 'Target] -> Environment -> MultiZipper RuleTag Grapheme -> [Int]
exceptionAppliesAtPoint target (ex1, ex2) mz = fmap fst $ flip runRuleAp mz $ do
    _ <- RuleAp $ matchMany Nothing ex1
    pos <- gets curPos
    MatchOutput{matchedGraphemes} <- RuleAp $ matchMany Nothing target
    _ <- RuleAp $ matchMany (listToMaybe matchedGraphemes) ex2
    return pos

-- | Given a 'Rule', determine if that rule matches. If so, for each
-- match, set the appropriate 'RuleTag's and return a tuple of @(is,
-- gs)@, where @gs@ is a list of matched 'Grapheme's, and @is@ is a
-- list of indices, one for each 'Category' lexeme matched.
matchRuleAtPoint
    :: Rule
    -> MultiZipper RuleTag Grapheme
    -> [(MatchOutput, MultiZipper RuleTag Grapheme)]
matchRuleAtPoint Rule{environment = (env1, env2), ..} mz = flip runRuleAp mz $ do
    _ <- RuleAp $ matchMany Nothing env1
    modify $ tag TargetStart
    matchResult <- RuleAp $ matchMany Nothing target
    modify $ tag TargetEnd
    _ <- RuleAp $ matchMany (listToMaybe $ matchedGraphemes matchResult) env2
    return matchResult

-- | Given a 'Rule', determine if the rule matches at the current
-- point; if so, apply the rule, adding appropriate tags.
applyOnce :: Rule -> StateT (MultiZipper RuleTag Grapheme) [] Bool
applyOnce r@Rule{target, replacement, exception} = do
    modify $ tag AppStart
    result <- try (matchRuleAtPoint r)
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
                    modify $ mkReplacement out replacement
                    return True
        Nothing -> return False

-- | Remove tags and advance the current index to the next 'Grapheme'
-- after the rule application.
setupForNextApplication :: Bool -> Rule -> MultiZipper RuleTag Grapheme -> Maybe (MultiZipper RuleTag Grapheme)
setupForNextApplication success r@Rule{flags=Flags{applyDirection}} = fmap untag .
    case applyDirection of
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
    in repeatRule (applyOnce r) startingPos
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
checkGraphemes (CategoriesDecl gs) = fmap $ \g -> if g `elem` gs then g else "\xfffd"

-- | Apply a 'Statement' to a 'MultiZipper'. This is a simple wrapper
-- around 'applyRule' and 'checkGraphemes'.
applyStatement :: Statement -> MultiZipper RuleTag Grapheme -> [MultiZipper RuleTag Grapheme]
applyStatement (RuleS r) mz = applyRule r mz
applyStatement (CategoriesDeclS gs) mz = [checkGraphemes gs mz]

-- | Apply a 'Rule' to a word, represented as a list of
-- 'Grapheme's. This is a simple wrapper around 'applyRule'.
applyRuleStr :: Rule -> [Grapheme] -> [[Grapheme]]
-- Note: 'fromJust' is safe here as 'apply' should always succeed
applyRuleStr r s = fmap toList $ applyRule r $ fromListStart s

-- | Apply a 'Statement' to a word, represented as a list of
-- 'Grapheme's. This is a simple wrapper around 'applyStatement'.
applyStatementStr :: Statement -> [Grapheme] -> [[Grapheme]]
applyStatementStr st s = fmap toList $ applyStatement st $ fromListStart s
