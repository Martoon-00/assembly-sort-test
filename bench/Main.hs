{-# LANGUAGE Rank2Types #-}

-- | Benchmarks.

import           Criterion.Main         (Benchmark, bench, defaultMain, envWithCleanup,
                                         nfIO, perRunEnv)
import           Test.QuickCheck        (arbitrary, resize, vectorOf)
import           Test.QuickCheck.Gen    (Gen (..))
import           Test.QuickCheck.Random (mkQCGen)
import           Universum

import           Asm.Data
import           Asm.Launcher

main :: IO ()
main = defaultMain
    [ -- launching process may be expensive operation, we track its time here
      fillingInputFileB "" $
          bench "void launch" $ nfIO $ launchProcess ""

    , let !entries = generate $ vectorOf 1000 $ resize 10 $ arbitrary @KeyValue
      in  fillingInputFileB (toInput entries) $
          -- TODO: throw on bad stderr
          bench "index build" $ nfIO $ launchProcess ""

    , let !entries = generate $ vectorOf 1 $ resize 10 arbitrary
          !queries = generate $ vectorOf 10000 $ someKeyOf entries
          !input   = toInput queries
      in  fillingInputFileB (toInput entries) $
          bench "queries read" $
            perRunEnv launchProcessWithInit $
            \interactionPhase -> interactionPhase input

    , let !entries = generate $ vectorOf 1000 $ resize 10 arbitrary
          !queries = generate $ vectorOf 10000 $ someKeyOf entries
          !input   = toInput queries
      in  fillingInputFileB (toInput entries) $
          bench "queries whole" $
            perRunEnv launchProcessWithInit $
            \interactionPhase -> interactionPhase input

    ]

envWithCleanup_ :: IO () -> IO () -> Benchmark -> Benchmark
envWithCleanup_ pre post = envWithCleanup pre (const post) . const

-- | Fills input file for the time of benchmark execution.
fillingInputFileB
    :: ProgramFileInput
    -> (InputFileFilled => Benchmark)
    -> Benchmark
fillingInputFileB = fillingInputFileWith envWithCleanup_

-- | Seed used to generate input data.
benchSeed :: Int
benchSeed = 32482346

-- | Generator runner fitting for benchmarking purposes.
generate :: NFData a => Gen a -> a
generate gen = force $ unGen gen (mkQCGen benchSeed) 100
