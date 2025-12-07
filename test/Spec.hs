module Main (main) where

import Test.Days.Day0
import Test.HUnit

tests :: Test
tests = test [testDay0]

main :: IO ()
main = runTestTTAndExit tests
