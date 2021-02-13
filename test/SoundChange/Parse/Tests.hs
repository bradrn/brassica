module SoundChange.Parse.Tests (tests) where

import Control.Monad (when)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import Data.Set (toList)
import Data.List.Split (divvy)

import SoundChange.Parse

tests :: TestTree
tests = testGroup "SoundChange.Parse"
    [ testGroup "tokeniseWord"
      [ testProperty "tokenises word to single characters by default" $ property $ do
          word <- forAll $ Gen.string (Range.linear 1 30) Gen.alpha
          tokeniseWords [] word === [Word $ fmap pure word]
      , testProperty "round-trips correctly" $ property $ do
          let gMaxLen = 4
          gs <- forAll $ fmap toList $ Gen.set (Range.linear 1 40) $ Gen.string (Range.linear 1 gMaxLen) Gen.alpha
          word <- forAll $ Gen.list (Range.linear 1 30) $ Gen.element gs
          -- precondition: no contiguous set of graphemes in 'word' can make up a larger grapheme
          let precondition = not $ flip any [2..gMaxLen] $ \gLen -> any (`elem` gs) (concat <$> divvy gLen 1 word)
          when precondition $ [Word word] === tokeniseWords gs (concat word)
      ]
    ]
