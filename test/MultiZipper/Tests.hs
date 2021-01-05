{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module MultiZipper.Tests (tests) where

import Data.Int (Int8)
import Data.Char (chr, ord)
import Data.Maybe (isNothing, fromJust)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Function as Fn
import Test.Tasty
import Test.Tasty.Hedgehog

import qualified Data.Set as S
import Safe

import MultiZipper

instance Fn.Arg Char where build = Fn.via ord chr
instance Fn.Vary Char where vary = Fn.contramap ord Fn.varyIntegral

genMZ :: forall ts a m. MonadGen m => Range Int -> m a -> m (MultiZipper ts a)
genMZ r a = (\(mz, _, _) -> mz) <$> genMZ' r a

genMZ' :: forall ts a m. MonadGen m => Range Int -> m a -> m (MultiZipper ts a, [a], Int)
genMZ' r a = do
    as <- Gen.list r a
    pos <- Gen.integral $ Range.constant 0 (length as)
    return (fromJust $ fromListPos as pos, as, pos)

tests :: TestTree
tests = testGroup "MultiZipper"
    [ testGroup "fromListStart"
      [ testProperty "trips list correctly" $ property $ do
          as <- forAll $ Gen.list (Range.linear 0 20) Gen.alpha
          as === toList (fromListStart as)
      , testProperty "positions zipper at start" $ property $ do
          as <- forAll $ Gen.list (Range.linear 0 20) Gen.alpha
          let mz = fromListStart as
          curPos mz === 0
      ]
    , testGroup "fromListPos"
      [ testProperty "trips & positions list correctly when in bounds" $ property $ do
          as <- forAll $ Gen.list (Range.linear 0 20) Gen.alpha
          pos <- forAll $ Gen.integral $ Range.constant 0 (length as)
          case fromListPos as pos of
              Nothing -> failure
              Just mz -> do
                  as === toList (fromListStart as)
                  curPos mz === pos
      , testProperty "fails when out of bounds" $ property $ do
          as <- forAll $ Gen.list (Range.linear 0 20) Gen.alpha
          pos <- forAll $ Gen.choice
              [ Gen.integral $ Range.exponential (-100) (-1)
              , Gen.integral $ Range.exponential (length as + 1) (10 * (length as + 1))
              ]
          assert $ isNothing $ fromListPos as pos
      ]
    , testGroup "atStart"
      [ testProperty "determines start correctly" $ property $ do
          mz <- forAll $ toBeginning <$> genMZ @() (Range.linear 0 20) Gen.alpha
          assert $ atStart mz
      , testProperty "does not determine end" $ property $ do
          let nonEmpty = not . null . toList
          mz <- forAll $ fmap toEnd $ Gen.filter nonEmpty $ genMZ @() (Range.linear 0 20) Gen.alpha
          assert $ not $ atStart mz
      ]
    , testGroup "atEnd"
      [ testProperty "determines end correctly" $ property $ do
          mz <- forAll $ toEnd <$> genMZ @() (Range.linear 0 20) Gen.alpha
          assert $ atEnd mz
      , testProperty "does not determine start" $ property $ do
          let nonEmpty = not . null . toList
          mz <- forAll $ fmap toBeginning $ Gen.filter nonEmpty $ genMZ @() (Range.linear 0 20) Gen.alpha
          assert $ not $ atEnd mz
      ]
    , testGroup "value"
      [ testProperty "fails at end of list" $ property $ do
          mz <- forAll $ toEnd <$> genMZ @() (Range.linear 0 20) Gen.alpha
          assert $ isNothing $ value mz
      , testProperty "is same as (!!)" $ property $ do
          (mz, as, pos) <- forAll $ genMZ' @() (Range.linear 0 20) Gen.alpha
          (as `atMay` pos) === value mz
      ]
    , testGroup "valueN"
      [ testProperty "is additive" $ property $ do
          mz <- forAll $ genMZ @() (Range.linear 0 20) Gen.alpha
          let l = length $ toList mz
          m <- forAll $ Gen.integral $ Range.exponential 0 l
          n <- forAll $ Gen.integral $ Range.exponential 0 l
          let sequential = do
                  (as, mz') <- valueN m mz
                  (bs, _  ) <- valueN n mz'
                  return $ as ++ bs
              atOnce = fst <$> valueN (m+n) mz
          sequential === atOnce
      , testProperty "reduces to `value`" $ property $ do
          mz <- forAll $ toEnd <$> genMZ @() (Range.linear 0 20) Gen.alpha
          (pure <$> value mz) === (fst <$> valueN 1 mz)
      ]
    , testGroup "locationOf"
      [ testProperty "returns correct location" $ property $ do
          (mz, _, pos) <- forAll $ genMZ' (Range.linear 0 20) Gen.alpha
          let mz' = tag () mz
          case locationOf () mz' of
              Nothing -> failure
              Just pos' -> pos === pos'
      ]
    , testGroup "move"
      [ testProperty "seeks to correct position when at beginning" $ property $ do
          mz <- forAll $ toBeginning <$> genMZ @() (Range.linear 0 20) Gen.alpha
          let len = length $ toList mz
          n <- forAll $ Gen.integral $ Range.constant 0 len
          case move n mz of
              Nothing -> failure
              Just mz' -> curPos mz' === n
      , testProperty "is invertible" $ property $ do
          (mz, _, pos) <- forAll $ genMZ' @() (Range.linear 0 20) Gen.alpha
          n <- forAll $ Gen.integral $ Range.constant 0 pos
          case move (-n) mz >>= move n of
              Nothing -> failure
              Just mz' -> curPos mz' === pos
      , testProperty "fails when moving too far" $ property $ do
          (mz, as, pos) <- forAll $ genMZ' @() (Range.linear 0 20) Gen.alpha
          let len = length as
              bwdSpace = pos        -- can move at most this distance backwards
              fwdSpace = len - pos  -- can move at most this distance forwards
          n <- forAll $ Gen.choice
              [ Gen.integral $ negate <$> Range.exponential (3*(bwdSpace+1)) (bwdSpace+1)
              , Gen.integral $            Range.exponential (fwdSpace+1) (3*(fwdSpace+1))
              ]
          assert $ isNothing $ move n mz
      ]
    , testGroup "toBeginning"
      [ testProperty "moves to correct position" $ property $ do
          mz <- forAll $ genMZ @() (Range.linear 0 20) Gen.alpha
          assert $ 0 == curPos (toBeginning mz)
      , testProperty "disallows backward movement" $ property $ do
          mz <- forAll $ genMZ @() (Range.linear 0 20) Gen.alpha
          assert $ isNothing $ bwd $ toBeginning mz
      ]
    , testGroup "toEnd"
      [ testProperty "does not correspond to a value" $ property $ do
          mz <- forAll $ genMZ @() (Range.linear 0 20) Gen.alpha
          assert $ isNothing $ value $ toEnd mz
      , testProperty "disallows forward movement" $ property $ do
          mz <- forAll $ genMZ @() (Range.linear 0 20) Gen.alpha
          assert $ isNothing $ fwd $ toEnd mz
      ]
    , testGroup "tag"
      [ testProperty "sets tags correctly" $ property $ do
          mz <- forAll $ genMZ (Range.linear 0 20) Gen.alpha
          ts <- forAll $ Gen.set (Range.linear 1 10) (Gen.integral $ Range.exponentialBounded @Int)
          let mz' = S.foldr tag mz ts
              ts' = query mz'
          ts === S.fromList ts'
      ]
    , testGroup "untagWhen"
      [ testProperty "unsets correct tags" $ property $ do
          mz <- forAll $ genMZ (Range.linear 0 20) Gen.alpha
          -- Use Int8 labels to increase chance of predicate applyint to some
          ts <- forAll $ Gen.set (Range.linear 1 10) (Gen.integral $ Range.exponentialBounded @Int8)
          p  <- Fn.forAllFn $ Fn.fn Gen.bool_
          let mz' = untagWhen p $ S.foldr tag mz ts
              ts' = query mz'
          collect $ S.size ts - length ts'   -- display number of labels deleted
          assert $ not $ any p ts'
      ]
    , testGroup "untag"
      [ testProperty "removes all tags" $ property $ do
          mz <- forAll $ genMZ (Range.linear 0 20) Gen.alpha
          ts <- forAll $ Gen.set (Range.linear 1 10) (Gen.integral $ Range.exponentialBounded @Int)
          let mz' = untag $ S.foldr tag mz ts
          assert $ null $ query mz'
      ]
    , testGroup "seek"
      [ testProperty "seeks to position of correct tag" $ property $ do
          (mz, as, pos) <- forAll $ genMZ' @Int (Range.linear 0 20) Gen.alpha
          n <- forAll $ Gen.integral $ Range.constant (-pos) (length as - pos)
          let mz' = tag 0 mz
          case move n mz' >>= (seek 0 . tag 1) of
              Nothing -> failure
              Just mz'' -> curPos mz'' === pos
      ]
    , testGroup "modifyBetween"
      [ testProperty "maintains cursor position" $ property $ do
          (mz, as, pos) <- forAll $ genMZ' @Int (Range.linear 0 20) Gen.alpha
          n <- forAll $ Gen.integral $ Range.constant 0 (max 0 $ length as - pos - 1)
          m <- forAll $ max 0 <$> Gen.choice
              [ Gen.integral $ Range.constant 0 (length as - pos - n)
              , Gen.integral $ Range.constant (-n-1) (-pos-n)
              ]
          let mz' = fromJust $ move m $ tag 1 $ fromJust $ move n $ tag 0 mz
          repl <- Fn.forAllFn $ Fn.fn $ Gen.list (Range.linear 0 20) Gen.alpha
          case modifyBetween (0,1) repl mz' of
              Nothing -> failure
              Just mz'' -> value mz' === value mz''
      , testProperty "maintains tag position" $ property $ do
          (mz, as, pos) <- forAll $ genMZ' @Int (Range.linear 0 20) Gen.alpha
          n <- forAll $ Gen.integral $ Range.constant 0 (max 0 $ length as - pos - 1)
          m <- forAll $ max 0 <$> Gen.choice
              [ Gen.integral $ Range.constant 0 (length as - pos - n)
              , Gen.integral $ Range.constant (-n-1) (-pos-n)
              ]
          let mz' = tag 2 $ fromJust $ move m $ tag 1 $ fromJust $ move n $ tag 0 mz
          repl <- Fn.forAllFn $ Fn.fn $ Gen.list (Range.linear 0 20) Gen.alpha
          case modifyBetween (0,1) repl mz' of
              Nothing -> failure
              Just mz'' -> assert $ 2 `elem` query mz''
      , testProperty "results in correct length distance" $ property $ do
          (mz, as, pos) <- forAll $ genMZ' @Int (Range.linear 0 20) Gen.alpha
          n <- forAll $ Gen.integral $ Range.constant 0 (max 0 $ length as - pos - 1)
          repl <- forAll $ Gen.string (Range.linear 0 20) Gen.alpha
          let mz' = tag 1 $ fromJust $ move n $ tag 0 mz
          case modifyBetween (0,1) (const repl) mz' of
              Nothing -> failure
              Just mz'' -> length (toList mz'') === length (toList mz') - n + length repl
      ]
    ]
