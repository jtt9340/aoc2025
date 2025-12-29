module Test.Days.Day0 (testDay0) where

import Days.Day
import Days.Day0
import Test.HUnit
import Test.Util

input :: Day0
input = Day0 [0 .. 10]

testPart1 :: Assertion
testPart1 = actual @?= expected
  where
    actual = part1 input
    expected = 55

testPart2 :: Assertion
testPart2 = actual @?= expected
  where
    actual = part2 input
    expected = 0

testDay0 :: Test
testDay0 =
    "Day 0"
        ~: [ "parseDay" ~: testParseDay "fixtures/day0-input.txt" input
           , "Part 1" ~: testPart1
           , "Part 2" ~: testPart2
           ]
