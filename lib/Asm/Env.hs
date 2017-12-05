-- | Environmental variables.
-- Here come all parameters we may want to pass to tests.

module Asm.Env
    ( programPath
    , inputPath
    , useOneQuery
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

-- | Path to input file which executable expects.
inputPath :: FilePath
inputPath = fromMaybe "./sort.in" $ env "INPUT_PATH"
{-# NOINLINE inputPath #-}

-- | No more than one query will be passed to program.
useOneQuery :: Bool
useOneQuery = flag "ONE_QUERY"
{-# NOINLINE useOneQuery #-}
