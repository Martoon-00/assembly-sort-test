-- | Properties on successful executions of assembly program.

module Test.Asm.CorrectSpec
    ( spec
    ) where

import           Test.Hspec            (Expectation, Spec, describe, expectationFailure,
                                        it)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck       (arbitrary, forAll, listOf1, resize)
import           Universum

import           Asm.Data
import           Asm.Launcher


spec :: Spec
spec = do
    describe "basic" $ do
        it "1 key-value entry, no queries" $
            launch "aa v1\n" "" `hasOnlyStdout` null

        it "1 key-value entry, 1 query" $
            launch "aa v1\n" "aa\n" `hasOnlyStdout` oneLine "v1"

        prop "few entires, few queries" $
            forAll (resize 10 $ listOf1 $ arbitrary @KeyValue) $
                \entries ->
            forAll (resize 10 $ someKeysOf entries) $
                \queries ->
            correctlySolves entries queries

    describe "special cases" $ do
        describe "newlines" $
            it "1 key-value pair, 1 query, no newline at end" $
                launch "aa v1\n" "aa" `hasOnlyStdout` oneLine "v1"

        describe "minmax" . modifyMaxSuccess (`div` 10) $ do
            it "no entries" $
                launch "" "" `hasOnlyStdout` null

            prop "plenty of entires, many queries" $
                forAll (resize 1000 $ listOf1 arbitrary) $
                    \entries ->
                forAll (resize 1000 $ someKeysOf entries) $
                    \queries ->
                correctlySolves entries queries

            prop "huge amount of small entries, few quires" $
                forAll (resize 10000 $ listOf1 $ resize 10 arbitrary) $
                    \entries ->
                forAll (resize 100 $ someKeysOf entries) $
                    \queries ->
                correctlySolves entries queries

            prop "few large entries, few queries" $
                forAll (resize 100 $ listOf1 $ resize 256 arbitrary) $
                    \entries ->
                forAll (resize 100 $ someKeysOf entries) $
                    \queries ->
                correctlySolves entries queries

            prop "few entries, tons of quires" $
                forAll (resize 100 $ listOf1 arbitrary) $
                    \entries ->
                forAll (resize 10000 $ someKeysOf entries) $
                    \queries ->
                correctlySolves entries queries

        prop "repeating keys" $
            forAll ((makeRepeatingKeys =<<) $ resize 10 $ listOf1 $ resize 10 arbitrary) $
                \entries ->
            forAll (resize 100 $ someKeysOf entries) $
                \queries ->
            correctlySolves entries queries

        prop "all ascii" $
            forAll (resize 4 $ listOf1 $ resize 10 $ genKeyValue withAsciiText) $
                \entries ->
            forAll (resize 10 $ someKeysOf entries) $
                \queries ->
            correctlySolves entries queries


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

correctlySolves :: [KeyValue] -> [Key] -> Expectation
correctlySolves entries queries =
    launch (toInput entries) (toInput queries)
        `hasOnlyStdout`
    (== buildList (solve entries queries))


oneLine :: Text -> Text -> Bool
oneLine line output = line <> "\n" == output

