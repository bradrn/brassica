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

import Control.Applicative ((<|>))
import Data.Function (on, (&))
import Data.List (sortBy)
import Data.Maybe (mapMaybe, fromMaybe, isJust, catMaybes, listToMaybe, maybeToList, fromJust)
import Data.Ord (Down(Down))

import Control.Monad.State

import MultiZipper
import SoundChange.Types

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
    { -- | Whether the matched lexeme is consumed or not. If 'False',
      -- the lexeme will need to undergo another round of matching.
      consumed         :: Bool
      -- | If a category was matched, the index of the matched
      -- grapheme in that category.
    , matchedCatIx     :: Maybe Int
      -- | The grapheme which was matched, if any.
    , matchedGrapheme  :: Maybe WordPart
    }

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
match _ Syllable     mz = (MatchOutput True Nothing Nothing,) <$> matchWordPart (Left SyllableBoundary) mz
match _ _            mz
    -- pass over 'SyllableBoundary', but only in the environment
    | SEnv <- singLT @a
    , Just mz' <- matchWordPart (Left SyllableBoundary) mz
    = Just (MatchOutput False Nothing (Just $ Left SyllableBoundary), mz')
match _ (Grapheme g) mz = (MatchOutput True Nothing (Just $ Right g),) <$> matchGrapheme g mz
match _ (Category gs) mz =
    gs
    -- Attempt to match each option in category...
    & fmap (\case
               BoundaryEl -> if atBoundary mz then Just (Nothing, mz) else Nothing
               GraphemeEl g -> (Just $ Right g,) <$> matchGrapheme g mz)
    -- ...get the index of each match...
    & zipWith (\i m -> fmap (i,) m) [0..]
    -- ...sort by match length descending...
    & sortBy (compare `on` Down . fmap (fmap length . fst . snd))
    -- ...and take the first match (=match with longest length).
    & (join . listToMaybe)
    & fmap (\(i, (g, mz')) -> (MatchOutput True (Just i) g, mz'))
match _ Boundary mz = if atBoundary mz then Just (MatchOutput True Nothing Nothing, mz) else Nothing
match prev (Optional l) mz = case matchMany prev l mz of
    Just (_, mz') -> Just (MatchOutput True Nothing Nothing, mz')
    Nothing       -> Just (MatchOutput True Nothing Nothing, mz)
match prev w@(Wildcard l) mz = case match prev l mz of
    Just r -> Just r
    Nothing -> fwd mz >>= match prev w
match prev Geminate mz = case prev of
    Nothing -> Nothing
    Just prev' -> (MatchOutput True Nothing (Just $ Right prev'),) <$> matchGrapheme prev' mz

matchGrapheme :: Grapheme -> MultiZipper t WordPart -> Maybe (MultiZipper t WordPart)
matchGrapheme g = matchWordPart (Right g)

matchWordPart :: WordPart -> MultiZipper t WordPart -> Maybe (MultiZipper t WordPart)
matchWordPart g mz = value mz >>= \cs -> if cs == g then fwd mz else Nothing

-- | Match a list of several 'Lexeme's against a
-- 'MultiZipper'. Arguments and output are the same as with 'match',
-- though the outputs are given as a list of indices and graphemes
-- rather than as a single index and grapheme. (Note that these lists
-- are in reverse order, which turns out to be more convenient for
-- some applications.)
matchMany ::
           ( OneOf a 'Target 'Env
           , SingLT a
           )
          => Maybe Grapheme
          -> [Lexeme a]
          -> MultiZipper t WordPart
          -> Maybe (([Int], [WordPart]), MultiZipper t WordPart)
matchMany = go [] []
  where
    go is gs _ [] mz = Just ((reverse $ catMaybes is, reverse $ catMaybes gs), mz)
    go is gs prev (l:ls) mz = match prev l mz >>= \case
        (MatchOutput{..}, mz') ->
            let ls' = if consumed then ls else l:ls
            in go (matchedCatIx:is) (matchedGrapheme:gs) ((matchedGrapheme >>= getGrapheme) <|> prev) ls' mz'

-- | Given a list of 'Lexeme's specifying a replacement, generate the
-- replacement as a 'String'.
mkReplacement
    :: [Int]                    -- ^ A list of indices, one for each 'Category' in the target.
    -> [WordPart]               -- ^ The 'WordPart's which were matched in the target.
    -> [Lexeme 'Replacement]    -- ^ The 'Lexeme's specifying the replacement.
    -> [WordPart]
mkReplacement = go []
  where
    go :: [WordPart] -> [Int] -> [WordPart] -> [Lexeme 'Replacement] -> [WordPart]
    go result _  _  []      = result
    go result is ins (l:ls) =
        let (is', r) = replaceLex is ins (lastMay result) l
        in go (result ++ r) is' ins ls

    lastMay l = if null l then Nothing else Just (last l)

    replaceLex :: [Int] -> [WordPart] -> Maybe WordPart -> Lexeme 'Replacement -> ([Int], [WordPart])
    replaceLex is     _   _    (Grapheme g)  = (is, [Right g])
    replaceLex (i:is) _   _    (Category gs) = (is,) $ pure $
        if i < length gs
        then case gs !! i of GraphemeEl g -> Right g
        else Right "\xfffd"  -- Unicode replacement character
    replaceLex []     _   _    (Category _)  = ([], [])   -- silently discard unmatchable categories
    replaceLex is     ins _    Metathesis    = (is, reverse ins)
    replaceLex is     _   prev Geminate      = (is, maybeToList prev)
    replaceLex is     _   _    Syllable      = (is, [Left SyllableBoundary])

-- | Given a 'Rule' and a 'MultiZipper', determines whether the
-- 'exception' of that rule (if any) applies starting at the current
-- position of the 'MultiZipper'; if it does, returns the index of the
-- first element of the matching 'target'.
exceptionAppliesAtPoint :: [Lexeme 'Target] -> Environment -> MultiZipper RuleTag WordPart -> Maybe Int
exceptionAppliesAtPoint target (ex1, ex2) mz = fmap fst $ flip runRuleAp mz $ do
    _ <- RuleAp $ matchMany Nothing ex1
    pos <- gets curPos
    (_, gs) <- RuleAp $ matchMany Nothing target
    _ <- RuleAp $ matchMany (listToMaybe $ mapMaybe getGrapheme gs) ex2
    return pos

-- | Given a 'Rule', determine if that rule matches. If so, set the
-- appropriate 'RuleTag's and return a tuple of @(is, gs)@, where @gs@
-- is a list of matched 'Grapheme's, and @is@ is a list of indices,
-- one for each 'Category' lexeme matched. If the rule does not match,
-- return 'Nothing'.
matchRuleAtPoint
    :: Rule
    -> MultiZipper RuleTag WordPart
    -> Maybe (([Int], [WordPart]), MultiZipper RuleTag WordPart)
matchRuleAtPoint Rule{environment = (env1, env2), ..} mz = flip runRuleAp mz $ do
    _ <- RuleAp $ matchMany Nothing env1
    modify $ tag TargetStart
    matchResult@(_,gs) <- RuleAp $ matchMany Nothing target
    modify $ tag TargetEnd
    _ <- RuleAp $ matchMany (listToMaybe $ mapMaybe getGrapheme gs) env2
    return matchResult

-- | Given a 'Rule', determine if the rule matches at the current
-- point; if so, apply the rule, adding appropriate tags.
applyOnce :: Rule -> State (MultiZipper RuleTag WordPart) Bool
applyOnce r@Rule{target, replacement, exception} = do
    modify $ tag AppStart
    result <- try (matchRuleAtPoint r)
    case result of
        Just (cats, gs) -> do
            exs <- case exception of
                Nothing -> pure []
                Just ex -> gets $ catMaybes . toList .
                    extend (exceptionAppliesAtPoint target ex)
            gets (locationOf TargetStart) >>= \p ->
                if maybe True (`elem` exs) p
                then return False
                else do
                    modifyMay $ modifyBetween (TargetStart, TargetEnd) (const $ mkReplacement cats gs replacement)
                    return True
        Nothing -> return False

-- | Remove tags and advance the current index to the next 'Grapheme'
-- after the rule application.
setupForNextApplication :: Bool -> Rule -> MultiZipper RuleTag WordPart -> Maybe (MultiZipper RuleTag WordPart)
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
