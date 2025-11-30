{-# LANGUAGE FunctionalDependencies #-}

module Days.Day (Day(..)) where

import Data.Text.Lazy (LazyText)

class (Show part1, Show part2) => Day a part1 part2 | a -> part1, a -> part2 where
    parseDay :: LazyText -> a
    part1 :: a -> part1
    part2 :: a -> part2
