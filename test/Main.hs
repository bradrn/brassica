module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified MultiZipper.Tests

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ MultiZipper.Tests.tests
    ]
