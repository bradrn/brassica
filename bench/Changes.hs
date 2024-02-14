{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Parallel.Strategies (withStrategy, parTraversable, rseq)
import Criterion.Main (defaultMain, bench, nf, bgroup, Benchmark)
import Data.FileEmbed (embedFile)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)

import Brassica.SoundChange
import Brassica.SoundChange.Frontend.Internal

main :: IO ()
main = defaultMain
    [ bgroup "single"
      [ bgroup "basic"
        [ bgroup "0" $ benchChanges basic ["b"]
        , bgroup "1" $ benchChanges basic ["a"]
        , bgroup "2" $ benchChanges basic ["a","b"]
        , bgroup "4" $ benchChanges basic ["a","b","x","a"]
        , bgroup "8" $ benchChanges basic ["a","b","x","a","x","b","a","b"]
        , bgroup "8a" $ benchChanges basic ["b","b","x","b","x","b","b","b"]
        ]
      , bgroup "complex"
        [ bgroup "1" $ benchChanges complex ["t"]
        , bgroup "2" $ benchChanges complex ["ti"]
        , bgroup "4" $ benchChanges complex ["a", "t", "i", "e"]
        , bgroup "8" $ benchChanges complex ["n", "y", "i", "u", "t", "i", "e", "a"]
        , bgroup "16" $ benchChanges complex ["a", "n", "y", "i", "u", "t", "i", "e", "d", "y", "i", "e", "t", "a", "d", "a"]
        , bgroup "16a" $ benchChanges complex ["u", "n", "y", "i", "u", "t", "i", "e", "d", "y", "a", "e", "t", "a", "d", "a"]
        ]
      ]
    , bgroup "many"
      [ bench "parse" $ nf parseSoundChanges manyChanges
      , bench "parseRun" $ case parseSoundChanges manyChanges of
            Left _ -> error "invalid changes file"
            Right cs -> nf (parseTokeniseAndApplyRules
                    fmap
                    cs
                    manyWords
                    Raw
                    (ApplyRules NoHighlight WordsOnlyOutput "/"))
                    Nothing
      ]
    ]
  where
    basic = Rule
        { target = [Grapheme "a"]
        , replacement = [Grapheme "b"]
        , environment = [([], [])]
        , exception = Nothing
        , flags = defFlags
        , plaintext = "a/b"
        }

    complex = Rule
        { target =
            [ Category $ FromElements $ Left <$> ["t", "d", "n"]
            , Optional [Grapheme "y"]
            , Category $ FromElements $ Left <$> ["i", "e"]
            ]
        , replacement =
            [ Category $ FromElements $ Left <$> ["c", "j", "nh"]
            , Optional [Geminate]
            ]
        , environment = pure
            ( [Category $ FromElements $ Left <$> [GBoundary, "a", "e", "i"]]
            , [Category $ FromElements $ Left <$> ["a", "e", "i", "o", "u"]]
            )
        , exception = Nothing
        , flags = defFlags
        , plaintext = "[t d n] (y) [i e] / [č j ñ] (>) / [# a e i] _ [a e i o u]"
        }

    benchChanges :: Rule Expanded -> PWord -> [Benchmark]
    benchChanges cs l =
        -- [ bench "log" $ nf (applyStatementWithLogs (RuleS cs)) l
        -- given the implementation of logging, the above benchmark doesn't help very much at all
        [ bench "nolog" $ nf (applyChanges [RuleS cs]) l
        ]

manyChanges :: String
manyChanges = unpack $ decodeUtf8 $(embedFile "bench/sample-changes.bsc")

manyWords :: String
manyWords = unpack $ decodeUtf8 $(embedFile "bench/sample-words.lex")

parFmap :: (a -> b) -> ParseOutput a -> ParseOutput b
parFmap f = withStrategy (parTraversable rseq) . fmap f
