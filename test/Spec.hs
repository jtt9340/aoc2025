module Main (main) where

import Test.HUnit
import Test.Days.Day0

tests :: Test
tests = test [ testDay0 ]

main :: IO ()
main = runTestTTAndExit tests
