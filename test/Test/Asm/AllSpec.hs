-- | Properties on assembly program.

module Test.Asm.AllSpec
    ( spec
    ) where

import           Test.Hspec            (Expectation, Spec, describe, expectationFailure,
                                        expectationFailure, it)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck       (Gen, arbitrary, elements, forAll, listOf, listOf1,
                                        property, resize)
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

        it "few entires, few queries" $ property $
            forAll (resize 10 $ listOf1 arbitrary) $
                \entries ->
            forAll (resize 10 $ someKeysOf entries) $
                \queries ->
            correctlySolves entries queries

    describe "edge cases" $ do
        describe "newlines" $
            it "1 key-value pair, 1 query, no newline at end" $
                launch "aa v1\n" "aa" `hasOnlyStdout` oneLine "v1"

        describe "minmax" . modifyMaxSuccess (* 10) $ do
            it "no entries" $
                launch "" "" `hasOnlyStdout` null

            it "plenty of entires, many queries" $ property $
                forAll (resize 1000 $ listOf1 arbitrary) $
                    \entries ->
                forAll (resize 1000 $ someKeysOf entries) $
                    \queries ->
                correctlySolves entries queries

            it "huge amount of small entries, few quires" $ property $
                forAll (resize 10000 $ listOf1 $ resize 10 arbitrary) $
                    \entries ->
                forAll (resize 100 $ someKeysOf entries) $
                    \queries ->
                correctlySolves entries queries

            it "few large entries, few queries" $ property $
                forAll (resize 100 $ listOf1 $ resize 10000 arbitrary) $
                    \entries ->
                forAll (resize 100 $ someKeysOf entries) $
                    \queries ->
                correctlySolves entries queries

            it "few entries, tons of quires" $ property $
                forAll (resize 100 $ listOf1 $ arbitrary) $
                    \entries ->
                forAll (resize 10000 $ someKeysOf entries) $
                    \queries ->
                correctlySolves entries queries


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

correctlySolves :: [KeyValue] -> [Key] -> Expectation
correctlySolves entries queries =
    launch (toInput entries) (toInput queries)
        `hasOnlyStdout`
    (== buildList (solve entries queries))


oneLine :: Text -> Text -> Bool
oneLine line output = line <> "\n" == output

someKeyOf :: [KeyValue] -> Gen Key
someKeyOf = fmap getKey . elements

someKeysOf :: [KeyValue] -> Gen [Key]
someKeysOf = listOf . someKeyOf
