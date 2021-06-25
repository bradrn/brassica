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
smallParadigm =
    [ NewFeature $ Feature Always Nothing
      [ Concrete [Prefix 1 "a"]
      , Concrete [Prefix 1 "b"]
      , Concrete [Prefix 1 "c"]
      ]
    , NewFeature $ Feature Always Nothing
      [ Concrete [Suffix 1 "x"]
      , Concrete [Suffix 1 "y"]
      , Concrete []
      ]
    , NewFeature $ Feature Always Nothing
      [ Concrete [Prefix 2 "n", Suffix 2 "v"]
      , Concrete [Prefix 2 "n", Suffix 2 "w"]
      , Concrete [Prefix 2 "m", Suffix 2 "w"]
      ]
    ]

largeParadigm :: Paradigm
largeParadigm =
    [ NewFeature $ Feature Always Nothing
      [ Concrete [Prefix 1 "a"]
      , Concrete [Prefix 1 "b"]
      , Concrete [Prefix 1 "c"]
      , Concrete [Prefix 1 "d"]
      , Concrete [Prefix 1 "e"]
      , Concrete [Prefix 1 "f"]
      , Concrete [Prefix 1 "g"]
      , Concrete [Prefix 1 "h"]
      , Concrete [Prefix 1 "i"]
      ]
    , NewFeature $ Feature Always Nothing
      [ Abstract "abstract1a"
      , Abstract "abstract1b"
      ]
    , NewFeature $ Feature Always Nothing
      [ Concrete [Prefix 2 "n", Suffix 2 "v"]
      , Concrete [Prefix 2 "n", Suffix 2 "w"]
      , Concrete [Prefix 2 "m", Suffix 2 "v"]
      , Concrete [Prefix 2 "m", Suffix 2 "w"]
      ]
    , NewFeature $ Feature Always Nothing
      [ Abstract "abstract2a"
      , Abstract "abstract2b"
      ]
    , NewFeature $ Feature Always Nothing
      [ Concrete [Suffix 4 "a"]
      , Concrete [Suffix 4 "b"]
      , Concrete [Suffix 4 "c"]
      , Concrete [Suffix 4 "d"]
      , Concrete [Suffix 4 "e"]
      , Concrete [Suffix 4 "f"]
      , Concrete [Suffix 4 "g"]
      , Concrete [Suffix 4 "h"]
      , Concrete [Suffix 4 "i"]
      ]
    , NewMapping ["abstract1a","abstract2a"] [Suffix 3 "q"]
    , NewMapping ["abstract1a","abstract2b"] [Suffix 3 "w"]
    , NewMapping ["abstract1b","abstract2a"] [Suffix 3 "e"]
    , NewMapping ["abstract1b","abstract2b"] [Suffix 3 "r"]
    ]

smallWords :: [String]
smallWords = ["word", "wurd", "ward", "werd", "wird"]

largeWords :: [String]
largeWords = ["word", "wurd", "ward", "werd", "wird", "xyz", "yzx", "zxy", "zyx", "yxz", "xzy", "qw", "we", "er", "rt", "ty"]
