-- | Properties on successful executions of assembly program.

module Test.Asm.SuccessSpec
    ( spec
    ) where

import           Formatting            (formatToString, stext, (%))
import           Test.Hspec            (Expectation, Spec, describe, expectationFailure,
                                        it)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck       (Property, Testable, arbitrary, forAll, listOf1,
                                        resize, vectorOf)
import           Universum

import           Asm.Data
import           Asm.Launcher


spec :: Spec
spec = do
    describe "basic" $ do
        it "1 key-value entry, no queries" $
            launch "aa v1\n" "" `hasOnlyStdoutWhichIs` emptyOutput

        it "1 key-value entry, 1 query" $
            launch "aa v1\n" "aa\n" `hasOnlyStdoutWhichIs` oneLine "v1"

        prop "few entires, few queries" $
            withClassicalInput correctlySolves

    describe "special cases" $ do
        describe "newlines" $ do
            it "no newline at end of query" $
                launch "aa v1\n" "aa" `hasOnlyStdoutWhichIs` oneLine "v1"

            it "no newline at end of file" $
                launch "aa v1" "aa\n" `hasOnlyStdoutWhichIs` oneLine "v1"

            it "newlines here and there in file" $
                withClassicalInput $
                    \entries queries ->
                forAll (variousNewlines $ toInput entries) $
                    \fileInput ->

                launch fileInput (toInput queries)
                    `hasOnlyStdout`
                buildList (solve entries queries)

            it "newlines here and there in queries" $
                withClassicalInput $
                    \entries queries ->
                forAll (variousNewlines $ toInput queries) $
                    \input ->

                launch (toInput entries) input
                    `hasOnlyStdout`
                buildList (solve entries queries)

        describe "minmax" . modifyMaxSuccess (`div` 10) $ do
            it "no entries" $
                launch "" "" `hasOnlyStdoutWhichIs` emptyOutput

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
            forAll (vectorOf 1 $ resize 10 $ someKeyOf entries) $
                \queries ->
            correctlySolves entries queries


launch :: ProgramFileInput -> ProgramInput -> IO ProgramOutput
launch fileInput input = fillingInputFile fileInput $ launchProcess input

-- | By default they generate with size 100, this may be unpleasant to debug.
withClassicalInput :: Testable prop => ([KeyValue] -> [Key] -> prop) -> Property
withClassicalInput mkProp =
    forAll (resize 10 $ listOf1 $ arbitrary @KeyValue) $
        \entries ->
    forAll (resize 10 $ someKeysOf entries) $
        \queries ->
    mkProp entries queries

type Predicate = (Text, ProgramStdout -> Bool)

hasOnlyStdoutWhichIs :: IO ProgramOutput -> Predicate -> Expectation
hasOnlyStdoutWhichIs launcher (expected, checker) = do
    ProgramOutput{..} <- launcher
    unless (null poStderr) $
        expectationFailure . toString $
            "Expected empty stderr, but got: " <> poStderr
    unless (checker poStdout) $
        expectationFailure $
        formatToString ("Got wrong stdout: "%stext%"\n  Expected: "%stext)
            poStdout expected

hasOnlyStdout :: IO ProgramOutput -> ProgramStdout -> Expectation
hasOnlyStdout launcher expected =
    hasOnlyStdoutWhichIs launcher (exactly expected)

correctlySolves :: [KeyValue] -> [Key] -> Expectation
correctlySolves entries queries =
    launch (toInput entries) (toInput queries)
        `hasOnlyStdout`
    buildList (solve entries queries)


oneLine :: Text -> Predicate
oneLine line = ("one line", \output -> line <> "\n" == output)

emptyOutput :: Predicate
emptyOutput = ("empty", null)

exactly :: Text -> Predicate
exactly a = (pretty a, (== a))
