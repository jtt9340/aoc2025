{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Util.Parsing (testParseInt) where

import Control.Exception
import Data.List (isInfixOf)
import Test.HUnit
import Util.Parsing

import qualified Data.Text.Lazy as L

assertThrowsWithMessage :: forall e a. (Exception e) => IO a -> String -> Assertion
assertThrowsWithMessage action message =
    try @e action >>= \case
        Left e -> assertBool "exception did not contain expected message" (message `isInfixOf` displayException e)
        Right _ -> assertFailure "action did not throw"

testParseInt :: Test
testParseInt =
    test
        . map TestCase
        $ [ (parseInt . L.pack $ "123") @?= 123
          , assertThrowsWithMessage @ErrorCall (evaluate . parseInt . L.pack $ "123g") "123g is not a number"
          , assertThrowsWithMessage @ErrorCall (evaluate . parseInt . L.pack $ "") "input does not start with a digit"
          ]
