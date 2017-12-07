-- | Datatypes which represent input/output passed to program.

module Asm.Data where

import qualified Data.Map            as M
import qualified Data.Text           as T
import qualified Data.Text.Buildable
import           Formatting          (bprint, build, (%))
import           Test.QuickCheck     (Arbitrary (..), Gen, elements, frequency, listOf,
                                      listOf1, sublistOf, suchThat)
import           Test.QuickCheck.Gen (choose, infiniteListOf)
import           Universum

import           Asm.Env
import           Asm.Process

type Key = Text
type Value = Text

-- | Single key-value entry
data KeyValue = KeyValue
    { getKey   :: Key
    , getValue :: Value
    } deriving (Show, Generic)

instance NFData KeyValue

instance Buildable KeyValue where
    build (KeyValue k v) = bprint (build%" "%build) k v

-- | Alias for more convenient enlistment
infix 5 @>
(@>) :: Text -> Text -> KeyValue
(@>) = KeyValue

-- | Solve given task
--   in two lines of code
solve :: [KeyValue] -> [Key] -> [Maybe Value]
solve entries queries =
    let entries' = M.fromList $ entries <&> \(KeyValue k v) -> (k, v)
    in  queries <&> flip M.lookup entries'

-- | Generator of key-value entry.
-- Uses given generator for key/value.
genKeyValue :: Gen Text -> Gen KeyValue
genKeyValue = join (liftM2 KeyValue) . (`suchThat` (not . null))

-- | Produces human-readable keys and values.
-- Use along with 'genKeyValue'.
withSimpleText :: Gen Text
withSimpleText = fmap toText . listOf $ elements (['a'..'z'] <> ['0'..'9'])

-- | Produces keys and values consisting of aribtrary ASCII characters,
-- excluding '\n', '\r' and some others.
-- Use along with 'genKeyValue'.
withAsciiText :: Gen Text
withAsciiText =
    fmap toText . listOf $ choose ('\0', '\255') `suchThat` noControlChars
  where
    noControlChars c = c < '\5' || c > ' '

instance Arbitrary KeyValue where
    arbitrary = genKeyValue withSimpleText

-- | Pick up some key among given entries.
someKeyOf :: [KeyValue] -> Gen Key
someKeyOf = fmap getKey . elements

-- | Pick up some keys among given entries.
someKeysOf :: [KeyValue] -> Gen [Key]
someKeysOf = multiplier . someKeyOf
  where
    multiplier
        | useOneQuery = \gen -> frequency [(1, pure []), (9, fmap pure gen)]
        | otherwise   = listOf

-- | Replaces used keys, choosing some keys subset and reassigning them
-- randomly to values.
makeRepeatingKeys :: [KeyValue] -> Gen [KeyValue]
makeRepeatingKeys entries
    | null entries = error "makeRepeatingKeys: can't do for empty list"
    | otherwise = do
        let keys = map getKey entries
        keys' <- sublistOf keys `suchThat` (not . null)
        forM entries $ \(KeyValue _ value) -> flip KeyValue value <$> elements keys'

-- | Print entries one per line.
toOutput :: Buildable a => [a] -> ProgramOutput __
toOutput = ProgramOutput . foldMap ((<> "\n") . pretty)

-- | Print entries one per line.
toInput :: Buildable a => [a] -> ProgramInput __
toInput = ProgramInput . foldMap ((<> "\n") . pretty)

-- | Remove last newline.
removeLastNewline :: ProgramInput a -> ProgramInput a
removeLastNewline (ProgramInput t) =
    ProgramInput $ T.drop 1 $ T.dropWhileEnd (/= '\n') t

-- | Insert arbitrary number of '\r's and '\n's instead of each '\n'.
variousNewlines :: ProgramInput a -> Gen (ProgramInput a)
variousNewlines (ProgramInput t) = do
    let pieces = T.split (== '\n') t
    (newlines : newlinesList) <-
        infiniteListOf (fmap toText $ listOf1 $ elements ['\n', '\r'])
    return . ProgramInput $
        mconcat (zipWith (<>) newlinesList pieces) <> newlines

