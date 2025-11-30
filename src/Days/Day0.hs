module Days.Day0 (Day0(..)) where

import Days.Day
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Read as LR

newtype Day0 = Day0 [Int]
  deriving (Show, Eq)

instance Day Day0 Int Int where
    parseDay = Day0 . map toDecimal . L.words
      where
        toDecimal text = case LR.decimal text of
          Left e -> error e
          Right (_, t) | not $ L.null t -> error $ L.unpack text ++ " is not a number"
          Right (i, _) -> i

    part1 (Day0 xs) = sum xs

    part2 (Day0 xs) = product xs
