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

solve :: [KeyValue] -> [Key] -> [Maybe Value]
solve entries queries =
    let entries' = M.fromList $ entries <&> \(KeyValue k v) -> (k, v)
    in  queries <&> flip M.lookup entries'

genKeyValue :: Gen Text -> Gen KeyValue
genKeyValue = join (liftM2 KeyValue) . (`suchThat` (not . null))

withSimpleText :: Gen Text
withSimpleText = fmap toText . listOf $ elements (['a'..'z'] <> ['0'..'9'])

withAsciiText :: Gen Text
withAsciiText = fmap toText . listOf $ choose ('\0', '\255') `suchThat` noControlChars
  where noControlChars c = c < '\5' || c > ' '

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

makeRepeatingKeys :: [KeyValue] -> Gen [KeyValue]
makeRepeatingKeys entries
    | null entries = error "makeRepeatingKeys: can't do for empty list"
    | otherwise = do
        let keys = map getKey entries
        keys' <- sublistOf keys `suchThat` (not . null)
        forM entries $ \(KeyValue _ value) -> flip KeyValue value <$> elements keys'

buildList :: Buildable a => [a] -> Text
buildList = foldMap ((<> "\n") . pretty)

toInput :: Buildable a => [a] -> ProgramInput
toInput = ProgramInput . foldMap ((<> "\n") . pretty)

removeLastNewline :: ProgramInput -> ProgramInput
removeLastNewline (ProgramInput t) =
    ProgramInput $ T.drop 1 $ T.dropWhileEnd (/= '\n') t

-- | Insert arbitrary number of '\r's and '\n's instead of each '\n'.
variousNewlines :: ProgramInput -> Gen ProgramInput
variousNewlines (ProgramInput t) = do
    let pieces = T.split (== '\n') t
    (newlines : newlinesList) <-
        infiniteListOf (fmap toText $ listOf1 $ elements ['\n', '\r'])
    return . ProgramInput $
        mconcat (zipWith (<>) newlinesList pieces) <> newlines

