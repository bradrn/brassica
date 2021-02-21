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
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module SoundChange.Apply
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
       , apply
       , applyStr
       ) where

import Control.Applicative (Applicative(liftA2), (<|>))
import Data.Function (on, (&))
import qualified Data.Foldable as F
import Data.List (sortBy)
import Data.Maybe (maybeToList, mapMaybe, fromMaybe, catMaybes, listToMaybe)
import Data.Ord (Down(Down))

import Control.Monad.State
import qualified Data.Map.Strict as Map

import MultiZipper
import SoundChange.Types
import Data.Functor ((<&>))
import Data.Bifunctor (Bifunctor(first))
import Data.Either (isLeft)

-- | Defines the tags used when applying a 'Rule'.
data RuleTag
    = AppStart     -- ^ The start of a rule application
    | TargetStart  -- ^ The start of the target
    | TargetEnd    -- ^ The end of the target
    deriving (Eq, Ord, Show)

-- | A monad in which to process a 'MultiZipper' over
-- 'Char's. Essentially a @StateT (MultiZipper RuleTag Grapheme) Maybe@:
-- it stores the 'MultiZipper' as state, and allows the possibility of
-- failure if a match attempt fails.
newtype RuleAp a = RuleAp { runRuleAp :: MultiZipper RuleTag WordPart -> Maybe (a, MultiZipper RuleTag WordPart) }
    deriving (Functor, Applicative, Monad, MonadFail, MonadState (MultiZipper RuleTag WordPart))
      via (StateT (MultiZipper RuleTag WordPart) Maybe)

-- | Lift a partial modification function into a 'State'. Update state
-- if it succeeds, otherwise rollback.
modifyMay :: (s -> Maybe s) -> State s ()
modifyMay f = modify $ \s -> fromMaybe s (f s)

-- | Given a partial stateful function, lift it into a 'State'
-- computation which returns 'Nothing' when the original action would
-- have failed.
try :: (s -> Maybe (a, s)) -> State s (Maybe a)
try p = state $ \s ->
    case p s of
        Just (a, s') -> (Just a,  s')
        Nothing      -> (Nothing, s)

-- | Describes the output of a 'match' operation.
data MatchOutput = MatchOutput
    { -- | For each category matched, the index of the matched
      -- grapheme in that category.
      matchedCatIxs    :: [Int]
      -- | For each optional group whether it matched or not
    , matchedOptionals :: [Bool]
      -- | The graphemes which were matched
    , matchedGraphemes :: [WordPart]
    } deriving (Show)

modifyMatchedGraphemes :: ([WordPart] -> [WordPart]) -> MatchOutput -> MatchOutput
modifyMatchedGraphemes f MatchOutput{..} = MatchOutput{matchedGraphemes=f matchedGraphemes, ..}

prependGrapheme :: WordPart -> MatchOutput -> MatchOutput
prependGrapheme g = modifyMatchedGraphemes (g:)

instance Semigroup MatchOutput where
    (MatchOutput a1 b1 c1) <> (MatchOutput a2 b2 c2) = MatchOutput (a1++a2) (b1++b2) (c1++c2)

-- | Match a single 'Lexeme' against a 'MultiZipper', and advance the
-- 'MultiZipper' past the match. Returns 'Nothing' if it does not
-- match; else return the 'MatchOutput' tupled with the updated
-- 'MultiZipper'.
match :: forall a t.
       ( OneOf a 'Target 'Env
       , SingLT a
       )
      => Maybe Grapheme       -- ^ The previously-matched grapheme, if any. (Used to match a 'Geminate'.)
      -> Lexeme a             -- ^ The lexeme to match.
      -> MultiZipper t WordPart   -- ^ The 'MultiZipper' to match against.
      -> Maybe (MatchOutput, MultiZipper t WordPart)
      -- ^ The output: a tuple @((i, g), mz)@ as described below.
match _    Syllable     mz = (MatchOutput [] [] [],) <$> matchWordPart isLeft mz
match prev (Optional l) mz = case matchMany prev l mz of
    Just (out, mz') -> Just (MatchOutput [] [True]  [] <> out, mz')
    Nothing         -> Just (MatchOutput [] [False] [], mz)
match prev w@(Wildcard l) mz = case match prev l mz of
    Just r -> Just r
    Nothing -> consume mz >>= \(g, mz') -> match prev w mz' <&> first (prependGrapheme g)
match prev w@(WithinSyllable l) mz = case match prev l mz of
    Just r -> Just r
    Nothing -> consume mz >>= \case
        (Left _, _) -> Nothing
        (g@(Right _), mz') -> match prev w mz' <&> first (prependGrapheme g)
match _ (Supra ss) mz = yank getSupras mz >>= \ssGiven ->
    if all (\(k, a) -> Map.lookup k ssGiven == a) ss
    then Just (MatchOutput [] [] [], mz)
    else Nothing
match prev l            mz
    -- pass over 'SyllableBoundary', but only in the environment, and
    -- only when the current lexeme is not a 'Syllable' (which should
    -- match) or else an 'Optional', 'Supra' or
    -- 'Wildcard'/'WithinSyllable' (which should try to match without
    -- consuming anything)
    | SEnv <- singLT @a
    , Just mz' <- matchWordPart isLeft mz
    = match prev l mz' <&> first (prependGrapheme $ Left (SyllableBoundary Map.empty))
match _ (Grapheme g) mz = (MatchOutput [] [] [Right g],) <$> matchGrapheme g mz
match _ (Category gs) mz =
    gs
    -- Attempt to match each option in category...
    & fmap (\case
               BoundaryEl -> if atBoundary mz then Just ([], mz) else Nothing
               GraphemeEl g -> ([Right g],) <$> matchGrapheme g mz)
    -- ...get the index of each match...
    & zipWith (\i m -> fmap (i,) m) [0..]
    -- ...sort by match length descending...
    & sortBy (compare `on` Down . fmap (fmap length . fst . snd))
    -- ...and take the first match (=match with longest length).
    & (join . listToMaybe)
    & fmap (\(i, (g, mz')) -> (MatchOutput [i] [] g, mz'))
match _ Boundary mz = if atBoundary mz then Just (MatchOutput [] [] [], mz) else Nothing
match prev Geminate mz = case prev of
    Nothing -> Nothing
    Just prev' -> (MatchOutput [] [] [Right prev'],) <$> matchGrapheme prev' mz

matchGrapheme :: Grapheme -> MultiZipper t WordPart -> Maybe (MultiZipper t WordPart)
matchGrapheme g = matchWordPart (==Right g)

matchWordPart :: (WordPart -> Bool) -> MultiZipper t WordPart -> Maybe (MultiZipper t WordPart)
matchWordPart p mz = value mz >>= \cs -> if p cs then fwd mz else Nothing

-- | Match a list of several 'Lexeme's against a
-- 'MultiZipper'. Arguments and output are the same as with 'match',
-- though the outputs are given as a list of indices and graphemes
-- rather than as a single index and grapheme.
matchMany ::
           ( OneOf a 'Target 'Env
           , SingLT a
           )
          => Maybe Grapheme
          -> [Lexeme a]
          -> MultiZipper t WordPart
          -> Maybe (MatchOutput, MultiZipper t WordPart)
matchMany = go (MatchOutput [] [] [])
  where
    go out _ [] mz = Just (out, mz)
    go out prev (l:ls) mz = match prev l mz >>= \case
        (out', mz') ->
            go (out <> out') ((lastMay (matchedGraphemes out') >>= getGrapheme) <|> prev) ls mz'

-- Small utility function, not exported
lastMay :: [a] -> Maybe a
lastMay l = if null l then Nothing else Just (last l)

-- | Given a list of 'Lexeme's specifying a replacement, generate the
-- replacement as a 'String'.
mkReplacement
    :: MatchOutput              -- ^ The result of matching against the target
    -> [Lexeme 'Replacement]    -- ^ The 'Lexeme's specifying the replacement.
    -> MultiZipper t WordPart
    -> MultiZipper t WordPart
mkReplacement = \out@MatchOutput{matchedGraphemes=matched} ls ->
    snd . go matched out ls
  where
    go _       out []     mz = (out, mz)
    go matched out (l:ls) mz =
        let (out', mz') = replaceLex out matched l mz
        in go matched out' ls mz'


    replaceLex
        :: MatchOutput
        -> [WordPart]
        -> Lexeme 'Replacement
        -> MultiZipper t WordPart
        -> (MatchOutput, MultiZipper t WordPart)
    replaceLex out _ (Grapheme g) mz = (out, insert (Right g) mz)
    replaceLex out@MatchOutput{matchedCatIxs=(i:is)} _ (Category gs) mz = (out{matchedCatIxs=is},) $ flip insert mz $
        if i < length gs
        then case gs !! i of GraphemeEl g -> Right g
        else Right "\xfffd"  -- Unicode replacement character
    replaceLex out@MatchOutput{matchedCatIxs=[]} _ (Category _) mz = (out,mz)   -- silently discard unmatchable categories
    replaceLex MatchOutput{matchedOptionals=(o:os), ..} matched (Optional ls) mz =
        let out' = MatchOutput{matchedOptionals=os, ..}
        in if o
           then go matched out' ls mz
           else (out', mz)
    replaceLex out@MatchOutput{matchedOptionals=[]} _ (Optional _) mz = (out,mz)   -- silently discard unmatchable optionals
    replaceLex out     matched Metathesis    mz = (out,) $ flip insertMany mz $ reverse matched
    replaceLex out     _       Geminate      mz = (out,) $ flip insertMany mz $ maybeToList $ lastMay $ matchedGraphemes out
    replaceLex out     _       Syllable      mz = (out,) $ flip insert mz $ Left (SyllableBoundary Map.empty)
    replaceLex out     _       (Supra ss)    mz = (out,) $
        flip zap mz $ \case
            Right _ -> Nothing
            Left (SyllableBoundary ss') -> Just $ Left $ SyllableBoundary $
                foldr (\(k, v) -> Map.alter (const v) k) ss' ss

-- | Given a 'Rule' and a 'MultiZipper', determines whether the
-- 'exception' of that rule (if any) applies starting at the current
-- position of the 'MultiZipper'; if it does, returns the index of the
-- first element of the matching 'target'.
exceptionAppliesAtPoint :: [Lexeme 'Target] -> Environment -> MultiZipper RuleTag WordPart -> Maybe Int
exceptionAppliesAtPoint target (ex1, ex2) mz = fmap fst $ flip runRuleAp mz $ do
    _ <- RuleAp $ matchMany Nothing ex1
    pos <- gets curPos
    MatchOutput{matchedGraphemes} <- RuleAp $ matchMany Nothing target
    _ <- RuleAp $ matchMany (listToMaybe $ mapMaybe getGrapheme matchedGraphemes) ex2
    return pos

-- | Given a 'Rule', determine if that rule matches. If so, set the
-- appropriate 'RuleTag's and return a tuple of @(is, gs)@, where @gs@
-- is a list of matched 'Grapheme's, and @is@ is a list of indices,
-- one for each 'Category' lexeme matched. If the rule does not match,
-- return 'Nothing'.
matchRuleAtPoint
    :: Rule
    -> MultiZipper RuleTag WordPart
    -> Maybe (MatchOutput, MultiZipper RuleTag WordPart)
matchRuleAtPoint Rule{environment = (env1, env2), ..} mz = flip runRuleAp mz $ do
    _ <- RuleAp $ matchMany Nothing env1
    modify $ tag TargetStart
    matchResult <- RuleAp $ matchMany Nothing target
    modify $ tag TargetEnd
    _ <- RuleAp $ matchMany (listToMaybe $ mapMaybe getGrapheme $ matchedGraphemes matchResult) env2
    return matchResult

-- | Given a 'Rule', determine if the rule matches at the current
-- point; if so, apply the rule, adding appropriate tags.
applyOnce :: Rule -> State (MultiZipper RuleTag WordPart) Bool
applyOnce r@Rule{target, replacement, exception} = do
    modify $ tag AppStart
    result <- try (matchRuleAtPoint r)
    case result of
        Just out -> do
            exs <- case exception of
                Nothing -> pure []
                Just ex -> gets $ catMaybes . toList .
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
setupForNextApplication :: Bool -> Rule -> MultiZipper RuleTag WordPart -> Maybe (MultiZipper RuleTag WordPart)
setupForNextApplication success Rule{flags=Flags{applyDirection}} = fmap untag .
    case applyDirection of
        RTL -> seek AppStart >=> bwd
        LTR ->
            if success
            then \mz -> do
                ts <- locationOf TargetStart mz
                te <- locationOf TargetEnd mz
                if ts == te
                    then -- need to move forward if applying an epenthesis rule to avoid an infinite loop
                        seek TargetEnd mz >>= fwd
                    else
                        seek TargetEnd mz
            else seek AppStart >=> fwd

-- | Apply a 'Rule' to a 'MultiZipper'. The application will start at
-- the beginning of the 'MultiZipper', and will be repeated as many
-- times as possible.
apply :: Rule -> MultiZipper RuleTag WordPart -> MultiZipper RuleTag WordPart
apply r = \mz ->    -- use a lambda so mz isn't shadowed in the where block
    let startingPos = case applyDirection $ flags r of
            LTR -> toBeginning mz
            RTL -> toEnd mz
    in repeatRule (applyOnce r) startingPos
  where
    repeatRule :: State (MultiZipper RuleTag WordPart) Bool -> MultiZipper RuleTag WordPart -> MultiZipper RuleTag WordPart
    repeatRule m mz = case runState m mz of
        (success, mz') ->
            if success && applyOnceOnly (flags r)
            then mz'
            else case setupForNextApplication success r mz' of
                Just mz'' -> repeatRule m mz''
                Nothing -> mz'

-- | Apply a 'Rule' to a word, represented as a list of
-- 'WordPart's. This is a simple wrapper around 'apply'.
applyStr :: Rule -> [WordPart] -> [WordPart]
-- Note: 'fromJust' is safe here as 'apply' should always succeed
applyStr r s = toList $ apply r $ fromListStart s
