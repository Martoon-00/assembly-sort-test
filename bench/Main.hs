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
          bench "void launch" $ nfIO $
          checkingOutput $
          launchProcess ""

    , let !entries = generate $ vectorOf 10000 $ resize 10 $ arbitrary @KeyValue
      in  fillingInputFileB (toInput entries) $
          bench "index build" $ nfIO $ launchProcess ""

    , let !entries = generate $ vectorOf 1 $ resize 10 arbitrary
          !queries = generate $ vectorOf 10000 $ someKeyOf entries
          !input   = toInput queries
      in  fillingInputFileB (toInput entries) $
          bench "queries read" $
            perRunEnv launchProcessWithInit $
            \interactionPhase -> interactionPhase input

    , let !entries = generate $ vectorOf 10000 $ resize 10 arbitrary
          !queries = generate $ vectorOf 100000 $ someKeyOf entries
          !input   = toInput queries
      in  fillingInputFileB (toInput entries) $
          bench "queries whole" $
            perRunEnv launchProcessWithInit $
            \interactionPhase -> interactionPhase input

    ]

-- | 'bracket_' version of 'envWithCleanup'.
envWithCleanup_ :: IO () -> IO () -> Benchmark -> Benchmark
envWithCleanup_ pre post = envWithCleanup pre (const post) . const

-- | Fills input file for the time of benchmark execution.
fillingInputFileB
    :: ProgramFileInput
    -> (InputFileFilled => Benchmark)
    -> Benchmark
fillingInputFileB = fillingInputFileWith envWithCleanup_

checkingOutput :: IO ProgramOutput -> IO ()
checkingOutput mkOutput =
    mkOutput >>= \ProgramOutput{..} -> do
        unless (null poStderr) $
            error $ "Some message in stderr: " <> poStderr

-- | Seed used to generate input data.
benchSeed :: Int
benchSeed = 32482346

-- | Generator runner fitting for benchmarking purposes.
generate :: NFData a => Gen a -> a
generate gen = force $ unGen gen (mkQCGen benchSeed) 100
