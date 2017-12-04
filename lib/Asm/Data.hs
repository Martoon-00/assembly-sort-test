-- | Datatypes which represent input/output passed to program.

module Asm.Data where

import qualified Data.Map            as M
import qualified Data.Text           as T
import qualified Data.Text.Buildable
import           Formatting          (bprint, build, (%))
import           Test.QuickCheck     (Arbitrary (..), Gen, elements, listOf, sublistOf,
                                      suchThat)
import           Test.QuickCheck.Gen (chooseAny, infiniteListOf)
import           Universum
import           Unsafe              (unsafeInit, unsafeLast)

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
withAsciiText = fmap toText . listOf $ chooseAny `suchThat` acceptableChar
  where acceptableChar c = all (/= c) [' ', '\n', '\r']

instance Arbitrary KeyValue where
    arbitrary = genKeyValue withSimpleText

someKeyOf :: [KeyValue] -> Gen Key
someKeyOf = fmap getKey . elements

someKeysOf :: [KeyValue] -> Gen [Key]
someKeysOf = listOf . someKeyOf

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

variousNewlines :: ProgramInput -> Gen ProgramInput
variousNewlines (ProgramInput t) = do
    let pieces = T.split (== '\n') t
    newlines' <- infiniteListOf (elements ['\n', '\r'])
    return . ProgramInput $
        mconcat (zipWith T.cons newlines' (unsafeInit pieces)) <>
        unsafeLast pieces

