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

import           Asm.Launcher
import           Test.Asm.Common

spec :: Spec
spec = do
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

launch :: ProgramFileInput -> ProgramInput -> IO (ExitCode, ProgramOutput)
launch fileInput input = fillingInputFile fileInput $ squashInit launchProcessExt input

hasOnlyStderr :: IO (ExitCode, ProgramOutput)
              -> ((ExitCode, ProgramStderr) -> Expectation)
              -> Expectation
hasOnlyStderr launcher checker = do
    (exitCode, ProgramOutput{..}) <- launcher
    unless (null poStdout) $
        expectationFailure . toString $
            "Expected empty stdout, but got: '" <> poStdout <> "'"
    checker (exitCode, poStderr)

-- | Combine checker for exit code and checker for stderr.
(&&&&)
    :: Predicate ExitCode
    -> Predicate ProgramStderr
    -> (ExitCode, ProgramStderr)
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

oneError :: Predicate ProgramStderr
oneError = "one line with error" >? \output ->
    case T.split (\c -> c == '\n' || c == '\r') output of
        [_, ""] -> True
        _       -> False

oneErrorWithLineNumber :: Word -> Predicate ProgramStderr
oneErrorWithLineNumber lineNum =
    ("one error at line " <> pretty lineNum) >? \output ->
        and
        [ predicateCond oneError output
        , pretty lineNum `T.isInfixOf` output
        ]

