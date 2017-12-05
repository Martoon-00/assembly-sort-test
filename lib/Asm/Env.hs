-- | Environmental variables.
-- Here come all parameters we may want to pass to tests.

module Asm.Env
    ( programPath
    , inputFile
    , useOneQuery
    , useLaxTests
    ) where

import           System.Environment (lookupEnv)
import           System.IO.Unsafe   (unsafePerformIO)
import           Universum

-- | Defines an environmental variable.
env :: String -> Maybe String
env = unsafePerformIO . lookupEnv

-- | 'True' if variable is specified.
flag :: String -> Bool
flag = isJust . env

-- | Path to tested executable.
programPath :: FilePath
programPath = fromMaybe "./sort" $ env "PROG_PATH"
{-# NOINLINE programPath #-}

-- | Name of file which executable expects.
inputFile :: FilePath
inputFile = fromMaybe "./sort.in" $ env "INPUT_FILE"
{-# NOINLINE inputFile #-}

-- | No more than one query will be passed to program.
useOneQuery :: Bool
useOneQuery = flag "ONE_QUERY"
{-# NOINLINE useOneQuery #-}

-- | Given requirements are quite vague.
-- Use this if test case fits only to particular version of program.
useLaxTests :: Bool
useLaxTests = flag "LAX_TESTS"
{-# NOINLINE useLaxTests #-}
