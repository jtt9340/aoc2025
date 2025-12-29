module Test.Util (testParseDay) where

import Days.Day
import Test.HUnit

import qualified Data.Text.Lazy.IO as LIO

testParseDay :: (Day a part1 part2) => FilePath -> a -> Assertion
testParseDay fixture input =
    LIO.readFile fixture >>= ((input @=?) . parseDay)
