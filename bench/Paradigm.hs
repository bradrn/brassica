module Main where

import Criterion.Main (defaultMain, bench, nf)

import SoundChange.Paradigm

main :: IO ()
main = defaultMain
    [ bench "small" $ nf (build smallParadigm) smallWords
    , bench "mid"   $ nf (build smallParadigm) largeWords
    , bench "mid2"  $ nf (build largeParadigm) smallWords
    , bench "large" $ nf (build largeParadigm) largeWords
    ]

smallParadigm :: Paradigm
smallParadigm = Paradigm
    [ Feature Nothing
      [ Concrete [Adfix (-1) "a"]
      , Concrete [Adfix (-1) "b"]
      , Concrete [Adfix (-1) "c"]
      ]
    , Feature Nothing
      [ Concrete [Adfix 1 "x"]
      , Concrete [Adfix 1 "y"]
      , Concrete [Null]
      ]
    , Feature Nothing
      [ Concrete [Adfix (-2) "n", Adfix 2 "v"]
      , Concrete [Adfix (-2) "n", Adfix 2 "w"]
      , Concrete [Adfix (-2) "m", Adfix 2 "w"]
      ]
    ] []

largeParadigm :: Paradigm
largeParadigm = Paradigm
    [ Feature Nothing
      [ Concrete [Adfix (-1) "a"]
      , Concrete [Adfix (-1) "b"]
      , Concrete [Adfix (-1) "c"]
      , Concrete [Adfix (-1) "d"]
      , Concrete [Adfix (-1) "e"]
      , Concrete [Adfix (-1) "f"]
      , Concrete [Adfix (-1) "g"]
      , Concrete [Adfix (-1) "h"]
      , Concrete [Adfix (-1) "i"]
      ]
    , Feature Nothing
      [ Abstract "abstract1a"
      , Abstract "abstract1b"
      ]
    , Feature Nothing
      [ Concrete [Adfix (-2) "n", Adfix 2 "v"]
      , Concrete [Adfix (-2) "n", Adfix 2 "w"]
      , Concrete [Adfix (-2) "m", Adfix 2 "v"]
      , Concrete [Adfix (-2) "m", Adfix 2 "w"]
      ]
    , Feature Nothing
      [ Abstract "abstract2a"
      , Abstract "abstract2b"
      ]
    , Feature Nothing
      [ Concrete [Adfix 4 "a"]
      , Concrete [Adfix 4 "b"]
      , Concrete [Adfix 4 "c"]
      , Concrete [Adfix 4 "d"]
      , Concrete [Adfix 4 "e"]
      , Concrete [Adfix 4 "f"]
      , Concrete [Adfix 4 "g"]
      , Concrete [Adfix 4 "h"]
      , Concrete [Adfix 4 "i"]
      ]
    ]
    [ (["abstract1a","abstract2a"], [Adfix 3 "q"])
    , (["abstract1a","abstract2b"], [Adfix 3 "w"])
    , (["abstract1b","abstract2a"], [Adfix 3 "e"])
    , (["abstract1b","abstract2b"], [Adfix 3 "r"])
    ]

smallWords :: [String]
smallWords = ["word", "wurd", "ward", "werd", "wird"]

largeWords :: [String]
largeWords = ["word", "wurd", "ward", "werd", "wird", "xyz", "yzx", "zxy", "zyx", "yxz", "xzy", "qw", "we", "er", "rt", "ty"]
