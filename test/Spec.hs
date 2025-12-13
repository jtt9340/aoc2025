module Main (main) where

import Test.Days.Day0
import Test.Days.Day1
import Test.HUnit
import Test.Util.Parsing

tests :: Test
tests = test [testParseInt, testDay0, testDay1]

main :: IO ()
main = runTestTTAndExit tests
