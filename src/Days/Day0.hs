module Days.Day0 (Day0 (..)) where

import Days.Day
import Util.Parsing

import qualified Data.Text.Lazy as L

newtype Day0 = Day0 [Int]
    deriving (Show, Eq)

instance Day Day0 Int Int where
    parseDay = Day0 . map parseInt . L.words
    part1 (Day0 xs) = sum xs
    part2 (Day0 xs) = product xs
