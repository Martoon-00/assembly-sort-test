-- | Properties on failing executions of assembly program.

module Test.Asm.UpsetSpec
    ( spec
    ) where

import           Test.Hspec            (Expectation, Spec, expectationFailure)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (elements, forAll, listOf)
import           Universum

import           Asm.Launcher



spec :: Spec
spec = do
    prop "no entries, 1 query" $
        forAll (fmap (ProgramInput . toText) $ listOf $ elements ['\n', '\r']) $
            \fileInput ->
        launch fileInput "aa\n" `hasOnlyStderr` const True


launch :: ProgramFileInput -> ProgramInput -> IO ProgramOutput
launch fileInput input = fillingInputFile fileInput $ launchProcess input

hasOnlyStderr :: IO ProgramOutput -> (ProgramStdout -> Bool) -> Expectation
hasOnlyStderr launcher checker = do
    ProgramOutput{..} <- launcher
    unless (null poStdout) $
        expectationFailure . toString $
            "Expected empty stdout, but got: '" <> poStdout <> "'"
    unless (checker poStderr) $
        expectationFailure $ "Got wrong stderr: '" <> toString poStdout <> "'"
