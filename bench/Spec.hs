import qualified Paradigm
import qualified SoundChange
import Criterion.Main (defaultMain)

main :: IO ()
main = defaultMain $ [Paradigm.benchmarks, SoundChange.benchmarks]