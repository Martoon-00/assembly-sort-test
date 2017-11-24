-- | Environmental variables.
-- Here come all parameters we may want to pass to tests.

module Asm.Env
    ( programPath
    , inputPath
    ) where

import           System.Environment (lookupEnv)
import           System.IO.Unsafe   (unsafePerformIO)
import           Universum

-- | Defines an environmental variable.
env :: String -> Maybe String
env = unsafePerformIO . lookupEnv


-- | Path to tested executable.
programPath :: FilePath
programPath = fromMaybe "./sort" $ env "PROG_PATH"
{-# NOINLINE programPath #-}

-- | Path to input file which executable expects.
inputPath :: FilePath
inputPath = fromMaybe "./sort.in" $ env "INPUT_PATH"
{-# NOINLINE inputPath #-}
