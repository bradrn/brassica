{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}

module MultiZipper.Tests (tests) where

import Data.Maybe (isNothing, fromJust)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import Safe

import MultiZipper

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
          mz <- forAll $ toEnd <$> genMZ @() (Range.linear 0 20) Gen.alpha
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
    ]
