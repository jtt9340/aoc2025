module Test.Days.Day1 (testDay1) where

import Days.Day
import Days.Day1
import Test.HUnit

import qualified Data.Text.Lazy.IO as LIO

input :: Day1
input = Day1 [-68, -30, 48, -5, 60, -55, -1, -99, 14, -82]

testParseInput :: Assertion
testParseInput = do
    inputFixture <- LIO.readFile "fixtures/day1-sample-input.txt"
    let parsedInput = parseDay inputFixture
    parsedInput @?= input

testPart1Sample :: Assertion
testPart1Sample = actual @?= expected
  where
    actual = part1 input
    expected = 3

testPart1 :: Assertion
testPart1 = do
    inputFixture <- LIO.readFile "fixtures/day1-input.txt"
    let parsedInput = parseDay inputFixture :: Day1
    let actual = part1 parsedInput
    actual @?= 1154

testDay1 :: Test
testDay1 =
    "Day 1"
        ~: [ "parseInput" ~: testParseInput
           , "testPart1Sample" ~: testPart1Sample
           , "testPart1" ~: testPart1
           ]
