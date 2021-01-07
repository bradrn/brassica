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
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module SoundChange.Apply
       ( -- * Types
         RuleTag(..)
       , RuleAp(..)
       , modifyMay
       , try
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
import Data.Maybe (isJust, catMaybes, listToMaybe, maybeToList, fromJust)
import Data.Ord (Down(Down))

import Control.Monad.State

import MultiZipper
import SoundChange.Types

-- | Defines the tags used when applying a 'Rule'.
data RuleTag
    = AppStart     -- ^ The start of a rule application
    | TargetStart  -- ^ The start of the target
    | TargetEnd    -- ^ The end of the target
    | Exception Int  -- ^ Exceptional position where rule should not be applied ('Int' argument is just an identifier to keep tags unique)
    deriving (Eq, Ord, Show)

-- | A monad in which to process a 'MultiZipper' over
-- 'Char's. Essentially a @StateT (MultiZipper RuleTag Grapheme) Maybe@:
-- it stores the 'MultiZipper' as state, and allows the possibility of
-- failure if a match attempt fails.
newtype RuleAp a = RuleAp { runRuleAp :: MultiZipper RuleTag Grapheme -> Maybe (a, MultiZipper RuleTag Grapheme) }
    deriving (Functor, Applicative, Monad, MonadFail, MonadState (MultiZipper RuleTag Grapheme))
      via (StateT (MultiZipper RuleTag Grapheme) Maybe)

-- | Lift a partial modification function into a 'RuleAp'.
modifyMay :: (MultiZipper RuleTag Grapheme -> Maybe (MultiZipper RuleTag Grapheme)) -> RuleAp ()
modifyMay f = RuleAp $ fmap ((),) . f

-- | Given a 'RuleAp' which may fail, modify it to always succeed,
-- returning 'Nothing' when the original action would have failed.
try :: RuleAp a -> RuleAp (Maybe a)
try (RuleAp p) = RuleAp $ \s ->
    case p s of
        Just (a, s') -> Just (Just a,  s')
        Nothing      -> Just (Nothing, s)

-- | Match a single 'Lexeme' against a 'MultiZipper', and advance the
-- 'MultiZipper' past the match. Returns 'Nothing' if it does not
-- match; else return a tuple @((i, g), mz)@, where @mz@ is the new
-- 'MultiZipper', and @g@ is the grapheme (if any) which was
-- matched. If the 'Lexeme' was a 'Category', @i@ is additionally the
-- index of the matched grapheme in that category; otherwise @i@ is
-- 'Nothing'.
match :: OneOf a 'Target 'Env
      => Maybe Grapheme       -- ^ The previously-matched grapheme, if any. (Used to match a 'Geminate'.)
      -> Lexeme a             -- ^ The lexeme to match.
      -> MultiZipper t Grapheme   -- ^ The 'MultiZipper' to match against.
      -> Maybe ((Maybe Int, Maybe Grapheme), MultiZipper t Grapheme)
      -- ^ The output: a tuple @((i, g), mz)@ as described below.
match _ (Grapheme g) mz = ((Nothing,Just g),) <$> matchGrapheme g mz
match _ (Category gs) mz =
    gs
    -- Attempt to match each option in category...
    & fmap (\case
               BoundaryEl -> if atBoundary mz then Just (Nothing, mz) else Nothing
               GraphemeEl g -> (Just g,) <$> matchGrapheme g mz)
    -- ...get the index of each match...
    & zipWith (\i m -> fmap (i,) m) [0..]
    -- ...sort by match length descending...
    & sortBy (compare `on` Down . fmap (fmap length . fst . snd))
    -- ...and take the first match (=match with longest length).
    & (join . listToMaybe)
    & fmap (\(i, (g, mz')) -> ((Just i, g), mz'))
match _ Boundary mz = if atBoundary mz then Just ((Nothing, Nothing), mz) else Nothing
match prev (Optional l) mz = case matchMany prev l mz of
    Just (_, mz') -> Just ((Nothing, Nothing), mz')
    Nothing       -> Just ((Nothing, Nothing), mz)
match prev Geminate mz = case prev of
    Nothing -> Nothing
    Just prev' -> ((Nothing,Just prev'),) <$> matchGrapheme prev' mz

matchGrapheme :: Grapheme -> MultiZipper t Grapheme -> Maybe (MultiZipper t Grapheme)
matchGrapheme g mz = value mz >>= \cs -> if cs == g then fwd mz else Nothing

-- | Match a list of several 'Lexeme's against a
-- 'MultiZipper'. Arguments and output are the same as with 'match',
-- though the outputs are given as a list of indices and graphemes
-- rather than as a single index and grapheme.
matchMany :: OneOf a 'Target 'Env
          => Maybe Grapheme
          -> [Lexeme a]
          -> MultiZipper t Grapheme
          -> Maybe (([Int], [Grapheme]), MultiZipper t Grapheme)
matchMany = go [] []
  where
    go is gs _ [] mz = Just ((reverse $ catMaybes is, reverse $ catMaybes gs), mz)
    go is gs prev (l:ls) mz = match prev l mz >>= \case
        ((i, g), mz') -> go (i:is) (g:gs) (g <|> prev) ls mz'

-- | Given a list of 'Lexeme's specifying a replacement, generate the
-- replacement as a 'String'.
mkReplacement
    :: [Int]                    -- ^ A list of indices, one for each 'Category' in the target.
    -> [Grapheme]               -- ^ The 'Grapheme's which were matched in the target.
    -> [Lexeme 'Replacement]    -- ^ The 'Lexeme's specifying the replacement.
    -> [Grapheme]
mkReplacement = go []
  where
    go :: [Grapheme] -> [Int] -> [Grapheme] -> [Lexeme 'Replacement] -> [Grapheme]
    go result _  _  []      = result
    go result is ins (l:ls) =
        let (is', r) = replaceLex is ins (lastMay result) l
        in go (result ++ r) is' ins ls

    lastMay l = if null l then Nothing else Just (last l)

    replaceLex :: [Int] -> [Grapheme] -> Maybe Grapheme -> Lexeme 'Replacement -> ([Int], [Grapheme])
    replaceLex is     _   _    (Grapheme g)  = (is, [g])
    replaceLex (i:is) _   _    (Category gs) = (is,) $ pure $
        if i < length gs
        then case gs !! i of GraphemeEl g -> g
        else "\xfffd"  -- Unicode replacement character
    replaceLex []     _   _    (Category _)  = ([], [])   -- silently discard unmatchable categories
    replaceLex is     ins _    Metathesis    = (is, reverse ins)
    replaceLex is     _   prev Geminate      = (is, maybeToList prev)

-- | Given a 'Rule' and a 'MultiZipper', determines whether the
-- 'exception' of that rule (if any) applies starting at the current
-- position of the 'MultiZipper'; if it does, returns the index of the
-- first element of the matching 'target'.
exceptionAppliesAtPoint :: [Lexeme 'Target] -> Environment -> MultiZipper RuleTag Grapheme -> Maybe Int
exceptionAppliesAtPoint target (ex1, ex2) mz = fmap fst $ flip runRuleAp mz $ do
    _ <- RuleAp $ matchMany Nothing ex1
    pos <- gets curPos
    (_, gs) <- RuleAp $ matchMany Nothing target
    _ <- RuleAp $ matchMany (listToMaybe gs) ex2
    return pos

-- | Given a 'Rule', determine if that rule matches. If so, set the
-- appropriate 'RuleTag's and return a tuple of @(is, gs)@, where @gs@
-- is a list of matched 'Grapheme's, and @is@ is a list of indices,
-- one for each 'Category' lexeme matched. If the rule does not match,
-- the computation fails.
--
-- In addition, a list of exceptional positions may be given. If the
-- target begins at one of the exceptional positions, the match will
-- fail automatically, even if the rest of the match would succeed.
matchRuleAtPoint ::  Rule -> RuleAp ([Int], [Grapheme])
matchRuleAtPoint Rule{environment = (env1, env2), ..} = do
    _ <- RuleAp $ matchMany Nothing env1
    modify $ tag TargetStart
    -- gets curPos >>= \pos -> when (pos `elem` exs) $ fail ""
    gets query >>= \ts -> when (hasException ts) $ fail ""
    matchResult@(_,gs) <- RuleAp $ matchMany Nothing target
    modify $ tag TargetEnd
    _ <- RuleAp $ matchMany (listToMaybe gs) env2
    return matchResult
  where
    hasException = any (\case { Exception _ -> True ; _ -> False })

-- | Given a list of exceptions and a 'Rule', determine if the rule
-- matches at the current point; if so, apply the rule, and advance
-- the current index to the next 'Grapheme' after the rule
-- application.
applyOnce :: Rule -> RuleAp Bool
applyOnce r@Rule{target, replacement} = do
    modify $ tag AppStart
    result <- try (matchRuleAtPoint r)
    modifyMay $ case result of
        Nothing -> seek AppStart >=> fwd
        Just (cats, gs) ->
            modifyBetween (TargetStart, TargetEnd) (const $ mkReplacement cats gs replacement)
            >=> seek TargetEnd
    modify $ untagWhen $ \case { Exception _ -> False ; _ -> True }

    return $ isJust result

withExceptions :: MultiZipper RuleTag a -> [Int] -> MultiZipper RuleTag a
withExceptions mz = fromJust . foldM (\mz ex -> tagAt (Exception ex) ex mz) mz

-- | Apply a 'Rule' to a 'MultiZipper'. The application will start at
-- the beginning of the 'MultiZipper', and will be repeated as many
-- times as possible.
apply :: Rule -> MultiZipper RuleTag Grapheme -> MultiZipper RuleTag Grapheme
apply r = \mz ->    -- use a lambda so mz isn't shadowed in the where block
    let exs = case exception r of
            Nothing -> []
            Just ex -> catMaybes $ toList $
                extend (exceptionAppliesAtPoint (target r) ex) mz
    in repeatRule (applyOnce r) $ flip withExceptions exs $ toBeginning mz
  where
    repeatRule :: RuleAp Bool -> MultiZipper RuleTag Grapheme -> MultiZipper RuleTag Grapheme 
    repeatRule m mz = case runRuleAp m mz of
        Just (success, mz') ->
            if success && null (target r)
            then -- need to move forward if applying an epenthesis rule to avoid an infinite loop
                case fwd mz' of
                    Just mz'advanced -> repeatRule m mz'advanced
                    Nothing          -> mz'
            else repeatRule m mz'
        _ -> mz

-- | Apply a 'Rule' to a word, represented as a 'String'. This is a
-- simple wrapper around 'apply'.
applyStr :: Rule -> [Grapheme] -> [Grapheme]
-- Note: 'fromJust' is safe here as 'apply' should always succeed
applyStr r s = toList $ apply r $ fromListStart s
