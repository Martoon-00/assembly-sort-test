{-# LANGUAGE Rank2Types #-}

-- | Launch of executable specified in task

module Asm.Launcher
    ( InputFileFilled
    , ProgramFileInput
    , ProgramInput (..)
    , ProgramOutput (..)
    , ProgramStdout
    , ProgramStderr
    , ProgramException
    , fillingInputFile
    , fillingInputFileWith

    , launchProcessExt
    , squashInit

    , launchProcessWithInit
    , launchProcess
    ) where

import           Data.Reflection     (Given, give)
import qualified Data.Text.Buildable
import           Formatting          (bprint, build, stext, (%))
import           System.Directory    (removeFile)
import           System.Exit         (ExitCode (..))
import           Universum

import           Asm.Env
import           Asm.Process

-- * Input file filling

data InputFileFilledUp = InputFileFilledUp

-- | Indicates that input file has been created and filled, and that
-- assembly program is safe to execute.
type InputFileFilled = Given InputFileFilledUp

-- | Key-value data executable should take from file.
type ProgramFileInput = ProgramInput

-- | Generalized version of 'fillingInputFile' to accept different
-- 'bracket_'-like functions.
fillingInputFileWith
    :: (IO () -> IO () -> b -> b)
    -> ProgramFileInput
    -> (InputFileFilled => b)
    -> b
fillingInputFileWith bracket_' (ProgramInput fileInput) act =
    bracket_' mkInputFile removeInputFile $ give InputFileFilledUp act
  where
    mkInputFile = writeFile inputPath fileInput
    removeInputFile = removeFile inputPath

-- | Fills expected input file 'inputPath' with given data upon performing
-- action.
--
-- This function not part of launching functions with intention, see its usages
-- in benchs.
fillingInputFile :: ProgramFileInput -> (InputFileFilled => IO a) -> IO a
fillingInputFile = fillingInputFileWith bracket_

-- * Launch

-- | Runtime error
data ProgramException = ProgramException
   { peExitCode :: Int
   , peStderr   :: Text
   } deriving (Show)

instance Exception ProgramException

instance Buildable ProgramException where
    build ProgramException{..} =
        bprint ("Program exited with "%build%"\nreason: "%stext)
        peExitCode peStderr

-- | High-level launcher, which passed 'programPath' derived from environment
-- and ensures that input file exists when executing.
-- For full explanation see 'launchProcessWithInit' below.
launchProcessExt
    :: InputFileFilled
    => IO (ProgramInput -> IO (ExitCode, ProgramOutput))
launchProcessExt = readCreateProcess programPath

-- | Launches executable specified by 'programPath'.
--
-- Once outer 'IO' is executed, process is started but no input fed.
-- When inner 'IO' is performed, process receives all input and its
-- termination is awaited. That's all for benchmarks' sake.
--
-- If program terminates with error, inner 'IO' will throw 'ProgramException'.
launchProcessWithInit
    :: InputFileFilled
    => IO (ProgramInput -> IO ProgramOutput)
launchProcessWithInit =
    launchProcessExt <&> \performInteraction input -> do
        (exitCode, output) <- performInteraction input
        case exitCode of
            ExitSuccess      -> pure output
            ExitFailure code -> throwM $ ProgramException code (poStderr output)

-- | Neglects decoupling between init and interaction phrases.
squashInit :: IO (a -> IO b) -> a -> IO b
squashInit launcher input = launcher >>= ($ input)

-- | Launches executable specified by 'programPath'.
--
-- If program terminates with error, 'ProgramException' will be thrown.
launchProcess :: InputFileFilled => ProgramInput -> IO ProgramOutput
launchProcess = squashInit launchProcessWithInit

