-- | Properties on failing executions of assembly program.

module Test.Asm.UpsetSpec
    ( spec
    ) where

import qualified Data.Text             as T
import           Formatting            (build, formatToString, shown, stext, (%))
import           System.Exit           (ExitCode (..))
import           Test.Hspec            (Expectation, Spec, describe, expectationFailure)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (elements, forAll, listOf)
import           Universum

import           Asm.Env
import           Asm.Launcher
import           Asm.Process
import           Test.Asm.Common

-- These tests execute only if 'LAX_TESTS' hasn't been passed,
-- because exit codes and where error messages are printed to may vary from
-- program to program.
spec :: Spec
spec = unless useLaxTests $ do
    prop "no entries, 1 query" $
        forAll (fmap (ProgramInput . toText) $ listOf $ elements ['\n', '\r']) $
            \fileInput ->
        launch fileInput "aa\n" `hasOnlyStderr` (okExit &&&& oneError)

    describe "emptiness" $ do
        prop "empty key in entries" $
            launch " v\n" "" `hasOnlyStderr` (errExit &&&& oneError)

        prop "empty value in entries" $
            launch "k \n" "" `hasOnlyStderr` (errExit &&&& oneError)

    describe "line numbers in errors" $ do
        prop "simple" $
            launch "a v1\nb v2\nc" ""
                `hasOnlyStderr` (errExit &&&& oneErrorWithLineNumber 3)

        prop "with empty lines" $
            launch "a v1\n\n\nb v2\nc" ""
                `hasOnlyStderr` (errExit &&&& oneErrorWithLineNumber 5)

launch :: ProgramInput FileInput
       -> ProgramInput Stdin
       -> IO (ExitCode, ProgramProduct)
launch fileInput input = fillingInputFile fileInput $ squashInit launchProcessExt input

hasOnlyStderr :: IO (ExitCode, ProgramProduct)
              -> ((ExitCode, ProgramOutput Stderr) -> Expectation)
              -> Expectation
hasOnlyStderr launcher checker = do
    (exitCode, ProgramProduct{..}) <- launcher
    let output = pretty poStdout
    unless (null output) $
        expectationFailure . toString $
            "Expected empty stdout, but got: '" <> output <> "'"
    checker (exitCode, poStderr)

-- | Combine checker for exit code and checker for stderr.
(&&&&)
    :: Predicate ExitCode
    -> Predicate (ProgramOutput Stderr)
    -> (ExitCode, ProgramOutput Stderr)
    -> Expectation
(Predicate ecDesc ecCheck) &&&& (Predicate errDesc errCheck) =
    \(ec, err) -> do
        unless (ecCheck ec) $
            expectationFailure $
                formatToString ("Got wrong error code "%shown%", expected "%stext)
                ec ecDesc

        unless (errCheck err) $
            expectationFailure $
                formatToString ("Got wrong stderr: "%build%"\n  Expected "%stext)
                err errDesc


okExit :: Predicate ExitCode
okExit = "ok exit" >? (== ExitSuccess)

errExit :: Predicate ExitCode
errExit = "errorneous exit" >? (/= ExitSuccess)

oneError :: Predicate (ProgramOutput Stderr)
oneError = "one line with error" >? \output ->
    case T.split (\c -> c == '\n' || c == '\r') (pretty output) of
        [_, ""] -> True
        _       -> False

oneErrorWithLineNumber :: Word -> Predicate (ProgramOutput Stderr)
oneErrorWithLineNumber lineNum =
    ("one error at line " <> pretty lineNum) >? \output ->
        and
        [ predicateCond oneError output
        , pretty lineNum `T.isInfixOf` pretty output
        ]

