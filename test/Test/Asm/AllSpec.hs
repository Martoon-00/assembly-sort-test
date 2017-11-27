-- | Properties on assembly program.

module Test.Asm.AllSpec
    ( spec
    ) where

import           Test.Hspec   (Expectation, Spec, describe, expectationFailure,
                               expectationFailure, it)
import           Universum

import           Asm.Launcher


spec :: Spec
spec = do
    describe "basic" $ do
        it "1 key-value pair, no queries" $
            launch "aa v1\n" "" `hasOnlyStdout` null

        it "1 key-value pair, 1 query" $
            launch "aa v1\n" "aa\n" `hasOnlyStdout` oneLine "v1"

        -- it "1 key-value pair, 1 query, no newline at end" $
        --     launch "aa v1\n" "aa" `hasOnlyStdout` oneLine "v1"

    describe "minmax" $ do
        return ()

    describe "error scenarious" $ do
        return ()


launch :: ProgramFileInput -> ProgramInput -> IO ProgramOutput
launch fileInput input = fillingInputFile fileInput $ launchProcess input

hasOnlyStdout :: IO ProgramOutput -> (ProgramStdout -> Bool) -> Expectation
hasOnlyStdout launcher checker = do
    ProgramOutput{..} <- launcher
    unless (null poStderr) $
        expectationFailure . toString $
            "Expected empty stderr, but got: " <> poStderr
    unless (checker poStdout) $
        expectationFailure $ "Got wrong stdout: '" <> toString poStdout <> "'"

oneLine :: Text -> Text -> Bool
oneLine line output = line <> "\n" == output
