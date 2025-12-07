module Test.Days.Day0 (testDay0) where

import Days.Day
import Days.Day0
import Test.HUnit

import qualified Data.Text.Lazy.IO as LIO

input :: Day0
input = Day0 [0 .. 10]

testParseInput :: Assertion
testParseInput = do
    inputFixture <- LIO.readFile "fixtures/day0-input.txt"
    let parsedInput = parseDay inputFixture
    parsedInput @?= input

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
    TestLabel
        "Day 0"
        ( TestList
            [ TestLabel "parseInput" (TestCase testParseInput)
            , TestLabel "Part 1" (TestCase testPart1)
            , TestLabel "Part 2" (TestCase testPart2)
            ]
        )
