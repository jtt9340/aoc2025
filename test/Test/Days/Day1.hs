module Test.Days.Day1 (testDay1) where

import Days.Day
import Days.Day1
import Test.HUnit
import Test.Util

import qualified Data.Text.Lazy.IO as LIO

input :: Day1
input = Day1 [-68, -30, 48, -5, 60, -55, -1, -99, 14, -82]

testPart1Sample :: Assertion
testPart1Sample = actual @?= expected
  where
    actual = part1 input
    expected = 3

testPart2Sample :: Assertion
testPart2Sample = actual @?= expected
  where
    actual = part2 input
    expected = 6

testPart1 :: Assertion
testPart1 = do
    inputFixture <- LIO.readFile "fixtures/day1-input.txt"
    let parsedInput = parseDay inputFixture :: Day1
    let actual = part1 parsedInput
    actual @?= 1154

testPart2 :: Assertion
testPart2 = do
    inputFixture <- LIO.readFile "fixtures/day1-input.txt"
    let parsedInput = parseDay inputFixture :: Day1
    let actual = part2 parsedInput
    actual @?= 6819

testDay1 :: Test
testDay1 =
    "Day 1"
        ~: [ "parseDay" ~: testParseDay "fixtures/day1-sample-input.txt" input
           , "Part 1 Sample" ~: testPart1Sample
           , "Part 2 Sample" ~: testPart2Sample
           , "Part 1" ~: testPart1
           , "Part 2" ~: testPart2
           ]
