-- | Common stuff for tests.

module Test.Asm.Common
    ( Predicate (..)
    , (>?)
    , exactly
    ) where

import           Universum

-- | Named checker.
data Predicate a = Predicate
    { predicateDesc :: Text
    , predicateCond :: a -> Bool
    }

-- | Convenient constructor alias.
infix 6 >?
(>?) :: Text -> (a -> Bool) -> Predicate a
(>?) = Predicate

exactly :: (Buildable a, Eq a) => a -> Predicate a
exactly a = pretty a >? (== a)
