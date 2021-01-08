{-# LANGUAGE ViewPatterns #-}

module SoundChange.Apply.Tests (tests) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit

import SoundChange.Apply
import SoundChange.Types

occurs :: [Grapheme] -> [Grapheme] -> Int
occurs needle (splitAt (length needle) -> (start, rest))
    | start == needle = 1 + occurs needle rest
    | null rest       = 0
    | otherwise       = occurs needle rest

shouldResultIn :: [Grapheme] -> [Grapheme] -> Rule -> Assertion
shouldResultIn str result rule = applyStr rule str @?= result

shouldResultIn' :: String -> String -> Rule -> Assertion
shouldResultIn' str result = shouldResultIn (pure <$> str) (pure <$> result)

tests :: TestTree
tests = testGroup "SoundChange.Apply"
    [ testGroup "simple unconditional sound changes (Grapheme only)"
      [ testProperty "do not alter input when identity" $ property $ do
          targ <- forAll $ Gen.list (Range.linear 1 3) $ Gen.string (Range.linear 1 3) Gen.lower
          str  <- forAll $ fmap concat $ Gen.list (Range.linear 1 30) $ Gen.frequency
              [ (3, pure <$> Gen.string (Range.linear 1 3) Gen.lower)
              , (1, Gen.constant targ)
              ]
          let rule = Rule (Grapheme <$> targ) (Grapheme <$> targ) ([],[]) Nothing
          let result = applyStr rule str
          str === result
      , testProperty "replaces every occurence" $ property $ do
          targ <- forAll $ Gen.list (Range.linear 1 3) $ Gen.string (Range.linear 1 3) Gen.lower
          repl <- forAll $ Gen.filter (/=targ) $ Gen.list (Range.linear 1 3) $ Gen.string (Range.linear 1 3) Gen.lower
          str  <- forAll $ fmap concat $ Gen.list (Range.linear 1 30) $ Gen.frequency
              [ (3, pure <$> Gen.string (Range.linear 1 3) Gen.lower)
              , (1, Gen.constant targ)
              ]
          let rule = Rule (Grapheme <$> targ) (Grapheme <$> repl) ([],[]) Nothing
          let result = applyStr rule str
          occurs targ result === occurs targ repl * occurs targ str
      , testCase "handles overlapping targets" $
          let rule = Rule (Grapheme . pure <$> "aba") (Grapheme . pure <$> "xxxx") ([],[]) Nothing
              str = pure <$> "ababababa"
          in applyStr rule str @?= pure <$> "xxxxbxxxxba"
      , testProperty "adds to every possible position in epenthesis" $ property $ do
          repl <- forAll $ Gen.list (Range.linear 1 3) $ Gen.string (Range.linear 1 3) Gen.lower
          str  <- forAll $ fmap concat $ Gen.list (Range.linear 1 30) $ pure <$> Gen.string (Range.linear 1 3) Gen.lower
          let rule = Rule [] (Grapheme <$> repl) ([],[]) Nothing
          let result = applyStr rule str
          length result === length str + (length repl * (1 + length str))
      ]
    , testGroup "simple conditional sound changes (Grapheme only)"
      [ testCase "‘before’ condition applies correctly" $ do
          "aacaababcax" `shouldResultIn'` "aacaababcax" $
              Rule [Grapheme "a"] [Grapheme "b"] ([Grapheme "x"],[]) Nothing

          "xaacaxabxabcax" `shouldResultIn'` "xbacaxbbxbbcax" $
              Rule [Grapheme "a"] [Grapheme "b"] ([Grapheme "x"],[]) Nothing

          "xaabcaxabxabcax" `shouldResultIn'` "xaabcaxbxxbxcax" $
              Rule [Grapheme "a", Grapheme "b"] [Grapheme "b", Grapheme "x"] ([Grapheme "x"],[]) Nothing

          "xaacaxabxabcax" `shouldResultIn'` "xbxacaxbxbxbxbcax" $
              Rule [Grapheme "a"] [Grapheme "b", Grapheme "x"] ([Grapheme "x"],[]) Nothing

          "xyaayacaxbxaxyabcayx" `shouldResultIn'` "xybayacaxbxaxybbcayx" $
              Rule [Grapheme "a"] [Grapheme "b"] ([Grapheme "x",Grapheme "y"],[]) Nothing

      , testCase "‘after’ condition applies correctly" $ do
          "xaacaababca" `shouldResultIn'` "xaacaababca" $
              Rule [Grapheme "a"] [Grapheme "b"] ([],[Grapheme "x"]) Nothing

          "aaxcaxabxabcax" `shouldResultIn'` "abxcbxabxabcbx" $
              Rule [Grapheme "a"] [Grapheme "b"] ([],[Grapheme "x"]) Nothing

          "aaxbcaxabxabcabx" `shouldResultIn'` "aaxbcaxbxxabcbxx" $
              Rule [Grapheme "a", Grapheme "b"] [Grapheme "b", Grapheme "x"] ([],[Grapheme "x"]) Nothing

          "aaxcaxabxabcax" `shouldResultIn'` "abxxcbxxabxabcbxx" $
              Rule [Grapheme "a"] [Grapheme "b", Grapheme "x"] ([],[Grapheme "x"]) Nothing

          "xyaayacaxbxaxyabcaxy" `shouldResultIn'` "xyaayacaxbxbxyabcbxy" $
              Rule [Grapheme "a"] [Grapheme "b"] ([],[Grapheme "x",Grapheme "y"]) Nothing
      ]
    ]
