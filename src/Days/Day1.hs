module Days.Day1 (Day1 (..)) where

import Data.Maybe
import Days.Day
import Util.Parsing

import qualified Data.Text.Lazy as L

newtype Day1 = Day1 [Int]
    deriving (Show, Eq)

instance Day Day1 Int Undefined where
    parseDay = Day1 . mapMaybe parseRotation . L.lines
      where
        parseRotation ('L' L.:< mag) = Just . negate . parseInt $ mag
        parseRotation ('R' L.:< mag) = Just . parseInt $ mag
        parseRotation _ = Nothing

    part1 (Day1 rotations) =
        snd
            $ foldl
                ( \(dial, password) rotation ->
                    let newDial = (dial + rotation) `mod` 100
                     in (newDial, if newDial == 0 then password + 1 else password)
                )
                (50, 0)
                rotations

    part2 = undefined
