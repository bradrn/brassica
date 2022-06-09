module SoundChange where

import DeepSeq ()
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as B8
import Criterion.Main ( bench, bgroup, env, nf, Benchmark )
import Brassica.SoundChange.Parse ( parseSoundChanges, errorBundlePretty )
import Control.Exception (Exception, throw)
import Brassica.SoundChange.Types ( SoundChanges )
import Control.Applicative ( Applicative(liftA2) )
import Brassica.SoundChange ( applyChanges, tokeniseAnd )
import Data.Bifunctor ( Bifunctor(bimap) )
import Brassica.SoundChange.Tokenise (detokeniseWords)

benchmarks :: Benchmark
benchmarks = bgroup "SoundChange benchmarks"
    [ env loadSCA $ bench "parsing SCA file" . nf parseSCA
    , env (parseSCA <$> loadSCA ## parseFile) $ bench "running SCA file" . nf (uncurry evolve)
    ]
    where
        loadSCA :: IO B.ByteString
        loadSCA = B.readFile "test/proto21e.bsc"
        parseSCA :: B.ByteString -> SoundChanges
        parseSCA = try . parseSoundChanges . B8.toString
        parseFile :: IO [String]
        -- Lazy IO should be fine here as the list will be evaluated to nf anyway
        parseFile = lines <$> readFile "test/proto21e.in"
        evolve :: SoundChanges -> [String] -> [Either String String]
        evolve sca texts = bimap errorBundlePretty detokeniseWords . tokeniseAnd applyChanges sca <$> texts
        infixr 3 ##
        (##) = liftA2 (,)



try :: (Exception e) => Either e a -> a
try = either throw id