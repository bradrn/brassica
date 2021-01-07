module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified MultiZipper.Tests
import qualified SoundChange.Apply.Tests

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ MultiZipper.Tests.tests
    , SoundChange.Apply.Tests.tests
    ]
